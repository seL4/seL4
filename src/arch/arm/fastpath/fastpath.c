/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <util.h>
#include <api/syscall.h>
#include <kernel/thread.h>
#include <machine/io.h>
#include <machine/profiler.h>
#include <machine/registerset.h>
#include <model/statedata.h>
#include <object/cnode.h>
#include <object/structures.h>
#include <config.h>
#include <assert.h>
#include <arch/fastpath/fastpath.h>
#include <armv/fastpath.h>
#include <arch/machine/registerset.h>

/* When building the fastpath the assembler in traps.S makes these
 * assumptions. Because compile_asserts are hard to do in assembler,
 * we place them here */
compile_assert(SysCall_Minus1, SysCall == -1)
compile_assert(SysReplyWait_Minus2, SysReplyWait == -2)

/* Return back to user. */
__attribute__((noreturn))
static inline void
fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{
    register word_t r0 asm ("r0") = badge;
    register word_t r1 asm ("r1") = msgInfo;
    asm volatile (
        "add sp, %[cur_thread], %[offset]\n\t"
        "ldmdb sp, {r2-lr}^\n\t"
        "rfeia sp\n\t"
        :
        : [offset] "i" (PT_LR_svc),
        [cur_thread] "r" (cur_thread),
        "r"(r0), "r"(r1)
        : "memory" );

    /*
     * We need to avoid a warning about returning in a noreturn
     * function.  gcc provides this neat builtin since 4.5,
     * __builtin_unreachable(), but using that currently generates a
     * slower fastpath on ARM11.  Should compilers change, we should
     * test that again.
     */

    while (1);
}

/* Fastpath cap lookup.  Returns a null_cap on failure. */
static inline cap_t FORCE_INLINE
lookup_fp(cap_t cap, cptr_t cptr)
{
    word_t cptr2;
    cte_t *slot;
    word_t radixBits, bits;
    word_t radix;

    if (unlikely(! cap_capType_equals(cap, cap_cnode_cap))) {
        return cap_null_cap_new();
    }

    bits = cap_cnode_cap_get_capCNodeGuardSize(cap);
    do {
        radixBits = cap_cnode_cap_get_capCNodeRadix(cap);
        cptr2 = cptr << bits;
        radix = cptr2 >> (32 - radixBits);
        slot = CTE_PTR(cap_cnode_cap_get_capCNodePtr(cap)) + radix;

        cap = slot->cap;
        bits += radixBits;
    } while (unlikely(bits < 32) && likely(cap_capType_equals(cap, cap_cnode_cap)));

    return cap;
}

static inline uint32_t
cap_page_directory_cap_get_capPDBasePtr_np(cap_t cap)
{
    return (cap.words[0] & 0xffffffe0);
}

/** DONT_TRANSLATE */
static inline void
clearExMonitor_fp(void)
{
    uint32_t temp1 = 0;
    uint32_t temp2;
    asm volatile (
        "strex %[output], %[mem], [%[mem]]"
        : [output]"+r"(temp1)
        : [mem]"r"(&temp2)
    );
}

static inline void FORCE_INLINE
switchToThread_fp(tcb_t *thread, pde_t *cap_pd, pde_t stored_hw_asid)
{
    hw_asid_t hw_asid;

    hw_asid = pde_pde_invalid_get_stored_hw_asid(stored_hw_asid);

    armv_contextSwitch_fp(cap_pd, hw_asid);

    *armKSGlobalsFrame = thread->tcbIPCBuffer;
    ksCurThread = thread;
    clearExMonitor_fp();
}

static inline void
thread_state_ptr_set_tsType_np(thread_state_t *ts_ptr, word_t tsType)
{
    ts_ptr->words[0] = tsType;
}

static inline void
thread_state_ptr_mset_blockingIPCEndpoint_tsType(thread_state_t *ts_ptr,
                                                 word_t ep_ref,
                                                 word_t tsType)
{
    ts_ptr->words[0] = ep_ref | tsType;
}

static inline void
thread_state_ptr_set_blockingIPCDiminish_np(thread_state_t *ts_ptr, word_t dim)
{
    ts_ptr->words[2] = dim;
}

static inline void
cap_reply_cap_ptr_new_np(cap_t *cap_ptr, word_t capCallerSlot)
{
    /* 1 is capReplyMaster */
    cap_ptr->words[1] = CTE_REF(capCallerSlot) | 1;
    cap_ptr->words[0] = cap_reply_cap;
}

static inline void
cap_reply_cap_ptr_new_np2(cap_t *cap_ptr, word_t isMaster, word_t capTCBPtr)
{
    cap_ptr->words[0] = TCB_REF(capTCBPtr) | cap_reply_cap;
    cap_ptr->words[1] = isMaster;
}

static inline void
endpoint_ptr_mset_epQueue_tail_state(endpoint_t *ep_ptr, word_t epQueue_tail,
                                     word_t state)
{
    ep_ptr->words[0] = epQueue_tail | state;
}

static inline void
endpoint_ptr_set_epQueue_head_np(endpoint_t *ep_ptr, word_t epQueue_head)
{
    ep_ptr->words[1] = epQueue_head;
}

static inline bool_t
isValidVTableRoot_fp(cap_t pd_cap)
{
    return (pd_cap.words[0] & MASK(4)) == cap_page_directory_cap;
}

/* This is an accelerated check that msgLength, which appears
   in the bottom of the msgInfo word, is <= 4 and that msgExtraCaps
   which appears above it is zero. We are assuming that n_msgRegisters == 4
   for this check to be useful. */
compile_assert (n_msgRegisters_eq_4, n_msgRegisters == 4)
static inline int
fastpath_mi_check(word_t msgInfo)
{
    return (msgInfo & MASK(seL4_MsgLengthBits + seL4_MsgExtraCapBits)) > 4;
}

static inline void
fastpath_copy_mrs(unsigned int length, tcb_t *src, tcb_t *dest)
{
    if (likely(!length)) {
        return;
    }

    assert(length <= 4);
    assert(msgRegisters[0] == 2);
    assert(msgRegisters[1] == 3);
    assert(msgRegisters[2] == 4);
    assert(msgRegisters[3] == 5);

    /* Unrolling the loop manually speeds up all 1-4-length IPCs. */
    if (length >= 1) {
        setRegister(dest, 2, getRegister(src, 2));
    }
    if (length >= 2) {
        setRegister(dest, 3, getRegister(src, 3));
    }
    if (length >= 3) {
        setRegister(dest, 4, getRegister(src, 4));
    }
    if (length >= 4) {
        setRegister(dest, 5, getRegister(src, 5));
    }
}

void
fastpath_call(word_t cptr, word_t msgInfo)
{
    message_info_t info;
    cap_t ep_cap;
    endpoint_t *ep_ptr;
    unsigned int length;
    tcb_t *dest;
    word_t badge;
    cte_t *replySlot, *callerSlot;
    cap_t newVTable;
    pde_t *cap_pd;
    pde_t stored_hw_asid;
    uint32_t fault_type;
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

    /* Check there is nothing waiting on the async endpoint */
    if (ksCurThread->boundAsyncEndpoint &&
            async_endpoint_ptr_get_state(ksCurThread->boundAsyncEndpoint) == AEPState_Active) {
        slowpath(SysCall);
    }

    /* Lookup the cap */
    ep_cap = lookup_fp(TCB_PTR_CTE_PTR(ksCurThread, tcbCTable)->cap, cptr);

    /* Check it's an endpoint */
    if (unlikely(! cap_capType_equals(ep_cap, cap_endpoint_cap) ||
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

    /* Get Page Directory. */
    cap_pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr_np(newVTable));

    blockingIPCDiminishCaps = thread_state_ptr_get_blockingIPCDiminishCaps(&dest->tcbState);

    /* Ensure that the destination has a valid VTable. */
    if (unlikely(! isValidVTableRoot_fp(newVTable))) {
        slowpath(SysCall);
    }

    /* Get HW ASID */
    stored_hw_asid = cap_pd[PD_ASID_SLOT];

    /* Ensure the destination has a higher/equal priority to us. */
    if (unlikely(dest->tcbPriority < ksCurThread->tcbPriority)) {
        slowpath(SysCall);
    }

    /* Ensure that the endpoint has standard non-diminishing rights. */
    if (unlikely(!cap_endpoint_cap_get_capCanGrant(ep_cap) ||
                 blockingIPCDiminishCaps)) {
        slowpath(SysCall);
    }

    if (unlikely(!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid))) {
        slowpath(SysCall);
    }

    /* Ensure the original caller is in the current domain and can be scheduled directly. */
    if (CONFIG_NUM_DOMAINS > 1 && unlikely(dest->tcbDomain != ksCurDomain)) {
        slowpath(SysCall);
    }

    /*
     * --- POINT OF NO RETURN ---
     *
     * At this stage, we have committed to performing the IPC.
     */

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
fastpath_reply_wait(word_t cptr, word_t msgInfo)
{
    message_info_t info;
    cap_t ep_cap;
    endpoint_t *ep_ptr;
    unsigned int length;
    cte_t *callerSlot;
    cte_t *replySlot;
    cap_t callerCap;
    tcb_t *caller;
    word_t badge;
    tcb_t *endpointTail;
    uint32_t fault_type;

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
        slowpath(SysReplyWait);
    }

    /* Lookup the cap */
    ep_cap = lookup_fp(TCB_PTR_CTE_PTR(ksCurThread, tcbCTable)->cap,
                       cptr);

    /* Check it's an endpoint */
    if (unlikely(!cap_capType_equals(ep_cap, cap_endpoint_cap) ||
                 !cap_endpoint_cap_get_capCanReceive(ep_cap))) {
        slowpath(SysReplyWait);
    }

    /* Check there is nothing waiting on the async endpoint */
    if (ksCurThread->boundAsyncEndpoint &&
            async_endpoint_ptr_get_state(ksCurThread->boundAsyncEndpoint) == AEPState_Active) {
        slowpath(SysReplyWait);
    }

    /* Get the endpoint address */
    ep_ptr = EP_PTR(cap_endpoint_cap_get_capEPPtr(ep_cap));
    callerSlot = TCB_PTR_CTE_PTR(ksCurThread, tcbCaller);
    callerCap = callerSlot->cap;

    /* Check that there's not a thread waiting to send */
    if (unlikely(endpoint_ptr_get_state(ep_ptr) == EPState_Send)) {
        slowpath(SysReplyWait);
    }

    /* Only reply if the reply cap is valid. */
    if (unlikely(callerCap.words[0] == 0)) {
        slowpath(SysReplyWait);
    }

    /* Determine who the caller is. */
    caller = TCB_PTR(cap_reply_cap_get_capTCBPtr(callerCap));

    /* Get reply slot from the caller */
    replySlot = TCB_PTR_CTE_PTR(caller, tcbReply);

    /* Get destination thread.*/
    newVTable = TCB_PTR_CTE_PTR(caller, tcbVTable)->cap;

    /* Get Page Directory. */
    cap_pd = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr_np(newVTable));

    /* Check that the caller has not faulted, in which case a fault
       reply is generated instead. */
    fault_type = fault_get_faultType(caller->tcbFault);
    if (unlikely(fault_type != fault_null_fault)) {
        slowpath(SysReplyWait);
    }

    /* Ensure that the destination has a valid MMU. */
    if (unlikely(! isValidVTableRoot_fp (newVTable))) {
        slowpath(SysReplyWait);
    }

    /* Get HWASID. */
    stored_hw_asid = cap_pd[PD_ASID_SLOT];

    /* Ensure the original caller can be scheduled directly. */
    if (unlikely(caller->tcbPriority < ksCurThread->tcbPriority)) {
        slowpath(SysReplyWait);
    }

    /* Ensure the HWASID is valid. */
    if (unlikely(!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid))) {
        slowpath(SysReplyWait);
    }

    /* Ensure the original caller is in the current domain and can be scheduled directly. */
    if (CONFIG_NUM_DOMAINS > 1 && unlikely(caller->tcbDomain != ksCurDomain)) {
        slowpath(SysReplyWait);
    }

    /*
     * --- POINT OF NO RETURN ---
     *
     * At this stage, we have committed to performing the IPC.
     */

    /* Set thread state to BlockedOnReceive */
    thread_state_ptr_mset_blockingIPCEndpoint_tsType(
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
