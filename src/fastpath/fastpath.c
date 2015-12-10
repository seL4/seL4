/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <fastpath/fastpath.h>

void
#ifdef ARCH_X86
FASTCALL NORETURN
#endif
fastpath_call(word_t cptr, word_t msgInfo)
{
    message_info_t info;
    cap_t ep_cap;
    endpoint_t *ep_ptr;
    word_t length;
    tcb_t *dest;
    word_t badge;
    cte_t *replySlot, *callerSlot;
    cap_t newVTable;
    pde_t *cap_pd;
    pde_t stored_hw_asid;
    word_t fault_type;
    uint32_t blockingIPCDiminishCaps;

    /*
     * Get message info, length, and fault type.
     *
     * Note that we don't ever need to read the contents of the
     * msgCapsUnwrapped field. However, when we return to the user, they should
     * be set to 0. We can save a few cycles by zeroing those bits out now.
     */
    info = message_info_set_msgCapsUnwrapped(messageInfoFromWord_raw(msgInfo), 0);
    length = message_info_get_msgLength(info);
    fault_type = fault_get_faultType(ksCurThread->tcbFault);

    /* Check there's no extra caps, the length is ok and there's no
     * saved fault. */
    if (unlikely(fastpath_mi_check(msgInfo) ||
                 fault_type != fault_null_fault)) {
        slowpath(SysCall);
    }

    /* Lookup the cap */
    ep_cap = lookup_fp(TCB_PTR_CTE_PTR(ksCurThread, tcbCTable)->cap, cptr);

    /* Check it's an endpoint */
    if (unlikely(!cap_capType_equals(ep_cap, cap_endpoint_cap) ||
                 !cap_endpoint_cap_get_capCanSend(ep_cap))) {
        slowpath(SysCall);
    }

    /* Get the endpoint address */
    ep_ptr = EP_PTR(cap_endpoint_cap_get_capEPPtr(ep_cap));

    /* Get the destination thread, which is only going to be valid
     * if the endpoint is valid. */
    dest = TCB_PTR(endpoint_ptr_get_epQueue_head(ep_ptr));

    /* Check that there's a thread waiting to receive */
    if (unlikely(endpoint_ptr_get_state(ep_ptr) != EPState_Recv)) {
        slowpath(SysCall);
    }

    /* Get destination thread.*/
    newVTable = TCB_PTR_CTE_PTR(dest, tcbVTable)->cap;

    /* Get vspace root. */
#if defined(ARCH_ARM) || !defined(CONFIG_PAE_PAGING)
    cap_pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(newVTable));
#else
    cap_pd = PDE_PTR(cap_pdpt_cap_get_capPDPTBasePtr(newVTable));
#endif
    blockingIPCDiminishCaps = thread_state_ptr_get_blockingIPCDiminishCaps(&dest->tcbState);

    /* Ensure that the destination has a valid VTable. */
    if (unlikely(! isValidVTableRoot_fp(newVTable))) {
        slowpath(SysCall);
    }

#ifdef ARCH_ARM
    /* Get HW ASID */
    stored_hw_asid = cap_pd[PD_ASID_SLOT];
#endif

    /* Ensure the destination has a higher/equal priority to us. */
    if (unlikely(dest->tcbPriority < ksCurThread->tcbPriority)) {
        slowpath(SysCall);
    }

    /* Ensure that the endpoint has standard non-diminishing rights. */
    if (unlikely(!cap_endpoint_cap_get_capCanGrant(ep_cap) ||
                 blockingIPCDiminishCaps)) {
        slowpath(SysCall);
    }

#ifdef ARCH_ARM
    if (unlikely(!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid))) {
        slowpath(SysCall);
    }
#endif

    /* Ensure the original caller is in the current domain and can be scheduled directly. */
    if (CONFIG_NUM_DOMAINS > 1 && unlikely(dest->tcbDomain != ksCurDomain)) {
        slowpath(SysCall);
    }

    /*
     * --- POINT OF NO RETURN ---
     *
     * At this stage, we have committed to performing the IPC.
     */

#ifdef ARCH_X86
    /* Need to update NextEIP in the calling thread */
    setRegister(ksCurThread, NextEIP, getRegister(ksCurThread, NextEIP) + 2);
#endif

    /* Dequeue the destination. */
    endpoint_ptr_set_epQueue_head_np(ep_ptr, TCB_REF(dest->tcbEPNext));
    if (unlikely(dest->tcbEPNext)) {
        dest->tcbEPNext->tcbEPPrev = NULL;
    } else {
        endpoint_ptr_mset_epQueue_tail_state(ep_ptr, 0, EPState_Idle);
    }

    badge = cap_endpoint_cap_get_capEPBadge(ep_cap);

    /* Block sender */
    thread_state_ptr_set_tsType_np(&ksCurThread->tcbState,
                                   ThreadState_BlockedOnReply);

    /* Get sender reply slot */
    replySlot = TCB_PTR_CTE_PTR(ksCurThread, tcbReply);

    /* Get dest caller slot */
    callerSlot = TCB_PTR_CTE_PTR(dest, tcbCaller);

    /* Insert reply cap */
    cap_reply_cap_ptr_new_np2(&callerSlot->cap, 0, TCB_REF(ksCurThread));
    cap_reply_cap_ptr_new_np(&replySlot->cap, CTE_REF(callerSlot));

    fastpath_copy_mrs (length, ksCurThread, dest);

    /* Dest thread is set Running, but not queued. */
    thread_state_ptr_set_tsType_np(&dest->tcbState,
                                   ThreadState_Running);
    switchToThread_fp(dest, cap_pd, stored_hw_asid);

    msgInfo = wordFromMessageInfo(info);
    fastpath_restore(badge, msgInfo, ksCurThread);
}

void
#ifdef ARCH_IA32
FASTCALL
#endif
fastpath_reply_recv(word_t cptr, word_t msgInfo)
{
    message_info_t info;
    cap_t ep_cap;
    endpoint_t *ep_ptr;
    word_t length;
    cte_t *callerSlot;
    cte_t *replySlot;
    cap_t callerCap;
    tcb_t *caller;
    word_t badge;
    tcb_t *endpointTail;
    word_t fault_type;

    cap_t newVTable;
    pde_t *cap_pd;
    pde_t stored_hw_asid;

    /*
     * Get message info, length, and fault type.
     *
     * Note that we don't ever need to read the contents of the
     * msgCapsUnwrapped field. However, when we return to the user, they should
     * be set to 0. We can save a few cycles by zeroing those bits out now.
     */
    info = message_info_set_msgCapsUnwrapped(messageInfoFromWord_raw(msgInfo), 0);
    length = message_info_get_msgLength(info);
    fault_type = fault_get_faultType(ksCurThread->tcbFault);

    /* Check there's no extra caps, the length is ok and there's no
     * saved fault. */
    if (unlikely(fastpath_mi_check(msgInfo) ||
                 fault_type != fault_null_fault)) {
        slowpath(SysReplyRecv);
    }

    /* Lookup the cap */
    ep_cap = lookup_fp(TCB_PTR_CTE_PTR(ksCurThread, tcbCTable)->cap,
                       cptr);

    /* Check it's an endpoint */
    if (unlikely(!cap_capType_equals(ep_cap, cap_endpoint_cap) ||
                 !cap_endpoint_cap_get_capCanReceive(ep_cap))) {
        slowpath(SysReplyRecv);
    }

    /* Check there is nothing waiting on the notification */
    if (ksCurThread->tcbBoundNotification &&
            notification_ptr_get_state(ksCurThread->tcbBoundNotification) == NtfnState_Active) {
        slowpath(SysReplyRecv);
    }

    /* Get the endpoint address */
    ep_ptr = EP_PTR(cap_endpoint_cap_get_capEPPtr(ep_cap));
    callerSlot = TCB_PTR_CTE_PTR(ksCurThread, tcbCaller);
    callerCap = callerSlot->cap;

    /* Check that there's not a thread waiting to send */
    if (unlikely(endpoint_ptr_get_state(ep_ptr) == EPState_Send)) {
        slowpath(SysReplyRecv);
    }

    /* Only reply if the reply cap is valid. */
    if (unlikely(!fastpath_reply_cap_check(callerCap))) {
        slowpath(SysReplyRecv);
    }

    /* Determine who the caller is. */
    caller = TCB_PTR(cap_reply_cap_get_capTCBPtr(callerCap));

    /* Get reply slot from the caller */
    replySlot = TCB_PTR_CTE_PTR(caller, tcbReply);

    /* Get destination thread.*/
    newVTable = TCB_PTR_CTE_PTR(caller, tcbVTable)->cap;

    /* Check that the caller has not faulted, in which case a fault
       reply is generated instead. */
    fault_type = fault_get_faultType(caller->tcbFault);
    if (unlikely(fault_type != fault_null_fault)) {
        slowpath(SysReplyRecv);
    }

    /* Get vspace root. */
#if defined(ARCH_ARM) || !defined(CONFIG_PAE_PAGING)
    cap_pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(newVTable));
#else
    cap_pd = PDE_PTR(cap_pdpt_cap_get_capPDPTBasePtr(newVTable));
#endif

    /* Ensure that the destination has a valid MMU. */
    if (unlikely(! isValidVTableRoot_fp (newVTable))) {
        slowpath(SysReplyRecv);
    }

#ifdef ARCH_ARM
    /* Get HWASID. */
    stored_hw_asid = cap_pd[PD_ASID_SLOT];
#endif

    /* Ensure the original caller can be scheduled directly. */
    if (unlikely(caller->tcbPriority < ksCurThread->tcbPriority)) {
        slowpath(SysReplyRecv);
    }

#ifdef ARCH_ARM
    /* Ensure the HWASID is valid. */
    if (unlikely(!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid))) {
        slowpath(SysReplyRecv);
    }
#endif

    /* Ensure the original caller is in the current domain and can be scheduled directly. */
    if (unlikely(caller->tcbDomain != ksCurDomain && maxDom)) {
        slowpath(SysReplyRecv);
    }

    /*
     * --- POINT OF NO RETURN ---
     *
     * At this stage, we have committed to performing the IPC.
     */

#ifdef ARCH_X86
    /* Need to update NextEIP in the calling thread */
    setRegister(ksCurThread, NextEIP, getRegister(ksCurThread, NextEIP) + 2);
#endif

    /* Set thread state to BlockedOnReceive */
    thread_state_ptr_mset_blockingObject_tsType(
        &ksCurThread->tcbState, (word_t)ep_ptr, ThreadState_BlockedOnReceive);
    thread_state_ptr_set_blockingIPCDiminish_np(
        &ksCurThread->tcbState, ! cap_endpoint_cap_get_capCanSend(ep_cap));

    /* Place the thread in the endpoint queue */
    endpointTail = TCB_PTR(endpoint_ptr_get_epQueue_tail(ep_ptr));
    if (likely(!endpointTail)) {
        ksCurThread->tcbEPPrev = NULL;

        /* Set head/tail of queue and endpoint state. */
        endpoint_ptr_set_epQueue_head_np(ep_ptr, TCB_REF(ksCurThread));
    } else {
        /* Append current thread onto the queue. */
        endpointTail->tcbEPNext = ksCurThread;
        ksCurThread->tcbEPPrev = endpointTail;
    }

    /* Update tail of queue. */
    endpoint_ptr_mset_epQueue_tail_state(ep_ptr, TCB_REF(ksCurThread),
                                         EPState_Recv);

    ksCurThread->tcbEPNext = NULL;

    /* Delete the reply cap. */
    cap_reply_cap_ptr_new_np(&replySlot->cap, CTE_REF(NULL));
    callerSlot->cap = cap_null_cap_new();

    /* I know there's no fault, so straight to the transfer. */

    /* Replies don't have a badge. */
    badge = 0;

    fastpath_copy_mrs (length, ksCurThread, caller);

    /* Dest thread is set Running, but not queued. */
    thread_state_ptr_set_tsType_np(&caller->tcbState,
                                   ThreadState_Running);
    switchToThread_fp(caller, cap_pd, stored_hw_asid);

    msgInfo = wordFromMessageInfo(info);
    fastpath_restore(badge, msgInfo, ksCurThread);
}
