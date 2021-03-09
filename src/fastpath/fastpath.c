/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <fastpath/fastpath.h>
#ifdef CONFIG_KERNEL_MCS
#include <object/reply.h>
#endif

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
#include <benchmark/benchmark_track.h>
#endif
#include <benchmark/benchmark_utilisation.h>

/* TODO: Add some checks from fastpath_call (e.g. valid VTable) */
#ifdef CONFIG_ARCH_ARM
static inline
#ifndef CONFIG_ARCH_ARM_V6
FORCE_INLINE
#endif
#endif
void NORETURN fastpath_signal(word_t cptr, word_t msgInfo)
{
    cap_t newVTable;
    vspace_root_t *cap_pd;
    pde_t stored_hw_asid;
    word_t fault_type;
    dom_t dom;

    /* Get fault type. */
    fault_type = seL4_Fault_get_seL4_FaultType(NODE_STATE(ksCurThread)->tcbFault);

    /* Check there's no extra caps, the length is ok and there's no
     * saved fault. */
    /* TODO: Probably only need to check for no fault here */
    if (unlikely(fastpath_mi_check(msgInfo) ||
                 fault_type != seL4_Fault_NullFault)) {
        slowpath(SysSend);
    }

    /* We land up in here on every SYSCALL_SEND, so we need to check that we are
     * actually signalling a notification */

    /* Lookup the cap */
    cap_t cap = lookup_fp(TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbCTable)->cap, cptr);

    /* Check it's a notification */
    if (!cap_capType_equals(cap, cap_notification_cap)) {
        slowpath(SysSend);
    }

    /* Get the notification address */
    notification_t *ntfnPtr = NTFN_PTR(cap_notification_cap_get_capNtfnPtr(cap));

    /* Get the notification state */
    uint32_t ntfnState = notification_ptr_get_state(ntfnPtr);

    /* Get the notification badge */
    word_t badge = cap_notification_cap_get_capNtfnBadge(cap);

    switch (ntfnState) {
        case NtfnState_Active: {
            word_t badge2 = notification_ptr_get_ntfnMsgIdentifier(ntfnPtr);
            badge2 |= badge;
            notification_ptr_set_ntfnMsgIdentifier(ntfnPtr, badge2);
            restore_user_context();
        }
        case NtfnState_Idle: {
            /* TODO: If bound thread is higher prio, switch to it directly (via switchToThread, rather than schedule). */
            tcb_t *tcb = (tcb_t *)notification_ptr_get_ntfnBoundTCB(ntfnPtr);
            /* Check if we are bound and that thread is waiting for a message */
            /* Note that the benchmarks currently do not measure signalling a bound thread, so this path isn't taken in sel4bench */
            if (tcb) {
                if (thread_state_ptr_get_tsType(&tcb->tcbState) == ThreadState_BlockedOnReceive) {
                    /* Send and start thread running */
                    cancelIPC(tcb);
                    setThreadState(tcb, ThreadState_Running);
                    setRegister(tcb, badgeRegister, badge);
                    if (tcb->tcbSchedContext == NULL) {
                        sched_context_t *sc = SC_PTR(notification_ptr_get_ntfnSchedContext(ntfnPtr));
                        if (sc != NULL && sc->scTcb == NULL) {
                            schedContext_donate(sc, tcb);
                            if (sc != NODE_STATE(ksCurSC)) {
                                /* refill_unblock_check should not be called on the
                                 * current SC as it is already running. The current SC
                                 * may have been bound to a notificaiton object if the
                                 * current thread was deleted in a long-running deletion
                                 * that became preempted. */
                                refill_unblock_check(sc);
                            }
                            schedContext_resume(sc);
                        }
                    }

                    if (isSchedulable(tcb)) {
                        possibleSwitchTo(tcb);
                    }

                    schedule();
                    activateThread();
                    restore_user_context();

                } else {
                    /* In particular, this path is taken when a thread
                     * is waiting on a reply cap since BlockedOnReply
                     * would also trigger this path. I.e, a thread
                     * with a bound notification will not be awakened
                     * by signals on that bound notification if it is
                     * in the middle of an seL4_Call.
                     */
                    notification_ptr_set_state(ntfnPtr, NtfnState_Active);
                    notification_ptr_set_ntfnMsgIdentifier(ntfnPtr, badge);
                    restore_user_context();
                }
            } else {
                notification_ptr_set_state(ntfnPtr, NtfnState_Active);
                notification_ptr_set_ntfnMsgIdentifier(ntfnPtr, badge);
                restore_user_context();
            }
        }
        case NtfnState_Waiting: {
            /* TODO: If unblocked thread is higher prio, switch to it directly (via switchToThread, rather than schedule). */
            tcb_queue_t ntfn_queue;
            tcb_t *dest;

            ntfn_queue.head = (tcb_t *)notification_ptr_get_ntfnQueue_head(ntfnPtr);
            ntfn_queue.end = (tcb_t *)notification_ptr_get_ntfnQueue_tail(ntfnPtr);

            dest = ntfn_queue.head;

            /* Haskell error "WaitingNtfn Notification must have non-empty queue" */
            assert(dest);

            /* Dequeue TCB */
            ntfn_queue = tcbEPDequeue(dest, ntfn_queue);
            notification_ptr_set_ntfnQueue_head(ntfnPtr, (word_t)ntfn_queue.head);
            notification_ptr_set_ntfnQueue_tail(ntfnPtr, (word_t)ntfn_queue.end);

            /* set the thread state to idle if the queue is empty */
            if (!ntfn_queue.head) {
                notification_ptr_set_state(ntfnPtr, NtfnState_Idle);
            }

            setThreadState(dest, ThreadState_Running);
            setRegister(dest, badgeRegister, badge);

            /* It looks like the threads in sel4bench do have scheduling contexts,
               so this path isn't taken in sel4bench */
            if (dest->tcbSchedContext == NULL) {
                sched_context_t *sc = SC_PTR(notification_ptr_get_ntfnSchedContext(ntfnPtr));
                if (sc != NULL && sc->scTcb == NULL) {
                    schedContext_donate(sc, dest);
                    if (sc != NODE_STATE(ksCurSC)) {
                        /* refill_unblock_check should not be called on the
                         * current SC as it is already running. The current SC
                         * may have been bound to a notificaiton object if the
                         * current thread was deleted in a long-running deletion
                         * that became preempted. */
                        refill_unblock_check(sc);
                    }
                    schedContext_resume(sc);
                }
            }

            /* Get destination thread.*/
            newVTable = TCB_PTR_CTE_PTR(dest, tcbVTable)->cap;

            /* Get vspace root. */
            cap_pd = cap_vtable_cap_get_vspace_root_fp(newVTable);

            /* Ensure that the destination has a valid VTable. */
            if (unlikely(! isValidVTableRoot_fp(newVTable))) {
                slowpath(SysSend);
            }

#ifdef CONFIG_ARCH_AARCH32
            /* Get HW ASID */
            stored_hw_asid = cap_pd[PD_ASID_SLOT];
#endif

            /* let gcc optimise this out for 1 domain */
            dom = maxDom ? ksCurDomain : 0;
            /* ensure only the idle thread or lower prio threads are present in the scheduler */
            if (unlikely(dest->tcbPriority < NODE_STATE(ksCurThread->tcbPriority) &&
                         !isHighestPrio(dom, dest->tcbPriority))) {
                slowpath(SysSend);
            }

#ifdef CONFIG_ARCH_AARCH32
            if (unlikely(!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid))) {
                slowpath(SysSend);
            }
#endif

            /* Ensure the destination thread is in the current domain and can be scheduled directly. */
            if (unlikely(dest->tcbDomain != ksCurDomain && maxDom)) {
                slowpath(SysSend);
            }

#ifdef ENABLE_SMP_SUPPORT
            /* Ensure both threads have the same affinity */
            if (unlikely(NODE_STATE(ksCurThread)->tcbAffinity != dest->tcbAffinity)) {
                slowpath(SysSend);
            }
#endif /* ENABLE_SMP_SUPPORT */

            /*
             * --- POINT OF NO RETURN ---
             */

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
            ksKernelEntry.is_fastpath = true;
#endif
            /* TODO: Figure out why this hangs during scheduler benchmarks

            // Dest thread is set Running, but not queued
            thread_state_ptr_set_tsType_np(&dest->tcbState,
                                           ThreadState_Running);
            setRegister(dest, badgeRegister, badge);
            switchToThread_fp(dest, cap_pd, stored_hw_asid);

            msgInfo = wordFromMessageInfo(seL4_MessageInfo_set_capsUnwrapped(info, 0));

            fastpath_restore(badge, msgInfo, NODE_STATE(ksCurThread));
            */

            /* It looks like this path is taken by the scheduler benchmarks */
            if (isSchedulable(dest)) {
                possibleSwitchTo(dest);
            }

            schedule();
            activateThread();
            restore_user_context();
        }
    }
    /* TODO: Why does fastpath_call not need this as well? */
    UNREACHABLE();
}

#ifdef CONFIG_ARCH_ARM
static inline
#ifndef CONFIG_ARCH_ARM_V6
FORCE_INLINE
#endif
#endif
void NORETURN fastpath_call(word_t cptr, word_t msgInfo)
{
    seL4_MessageInfo_t info;
    cap_t ep_cap;
    endpoint_t *ep_ptr;
    word_t length;
    tcb_t *dest;
    word_t badge;
    cap_t newVTable;
    vspace_root_t *cap_pd;
    pde_t stored_hw_asid;
    word_t fault_type;
    dom_t dom;

    /* Get message info, length, and fault type. */
    info = messageInfoFromWord_raw(msgInfo);
    length = seL4_MessageInfo_get_length(info);
    fault_type = seL4_Fault_get_seL4_FaultType(NODE_STATE(ksCurThread)->tcbFault);

    /* Check there's no extra caps, the length is ok and there's no
     * saved fault. */
    if (unlikely(fastpath_mi_check(msgInfo) ||
                 fault_type != seL4_Fault_NullFault)) {
        slowpath(SysCall);
    }

    /* Lookup the cap */
    ep_cap = lookup_fp(TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbCTable)->cap, cptr);

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

    /* ensure we are not single stepping the destination in ia32 */
#if defined(CONFIG_HARDWARE_DEBUG_API) && defined(CONFIG_ARCH_IA32)
    if (unlikely(dest->tcbArch.tcbContext.breakpointState.single_step_enabled)) {
        slowpath(SysCall);
    }
#endif

    /* Get destination thread.*/
    newVTable = TCB_PTR_CTE_PTR(dest, tcbVTable)->cap;

    /* Get vspace root. */
    cap_pd = cap_vtable_cap_get_vspace_root_fp(newVTable);

    /* Ensure that the destination has a valid VTable. */
    if (unlikely(! isValidVTableRoot_fp(newVTable))) {
        slowpath(SysCall);
    }

#ifdef CONFIG_ARCH_AARCH32
    /* Get HW ASID */
    stored_hw_asid = cap_pd[PD_ASID_SLOT];
#endif

#ifdef CONFIG_ARCH_X86_64
    /* borrow the stored_hw_asid for PCID */
    stored_hw_asid.words[0] = cap_pml4_cap_get_capPML4MappedASID_fp(newVTable);
#endif

#ifdef CONFIG_ARCH_IA32
    /* stored_hw_asid is unused on ia32 fastpath, but gets passed into a function below. */
    stored_hw_asid.words[0] = 0;
#endif
#ifdef CONFIG_ARCH_AARCH64
    stored_hw_asid.words[0] = cap_vtable_root_get_mappedASID(newVTable);
#endif

#ifdef CONFIG_ARCH_RISCV
    /* Get HW ASID */
    stored_hw_asid.words[0] = cap_page_table_cap_get_capPTMappedASID(newVTable);
#endif

    /* let gcc optimise this out for 1 domain */
    dom = maxDom ? ksCurDomain : 0;
    /* ensure only the idle thread or lower prio threads are present in the scheduler */
    if (unlikely(dest->tcbPriority < NODE_STATE(ksCurThread->tcbPriority) &&
                 !isHighestPrio(dom, dest->tcbPriority))) {
        slowpath(SysCall);
    }

    /* Ensure that the endpoint has has grant or grant-reply rights so that we can
     * create the reply cap */
    if (unlikely(!cap_endpoint_cap_get_capCanGrant(ep_cap) &&
                 !cap_endpoint_cap_get_capCanGrantReply(ep_cap))) {
        slowpath(SysCall);
    }

#ifdef CONFIG_ARCH_AARCH32
    if (unlikely(!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid))) {
        slowpath(SysCall);
    }
#endif

    /* Ensure the original caller is in the current domain and can be scheduled directly. */
    if (unlikely(dest->tcbDomain != ksCurDomain && maxDom)) {
        slowpath(SysCall);
    }

#ifdef CONFIG_KERNEL_MCS
    if (unlikely(dest->tcbSchedContext != NULL)) {
        slowpath(SysCall);
    }

    reply_t *reply = thread_state_get_replyObject_np(dest->tcbState);
    if (unlikely(reply == NULL)) {
        slowpath(SysCall);
    }
#endif

#ifdef ENABLE_SMP_SUPPORT
    /* Ensure both threads have the same affinity */
    if (unlikely(NODE_STATE(ksCurThread)->tcbAffinity != dest->tcbAffinity)) {
        slowpath(SysCall);
    }
#endif /* ENABLE_SMP_SUPPORT */

    /*
     * --- POINT OF NO RETURN ---
     *
     * At this stage, we have committed to performing the IPC.
     */

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
    ksKernelEntry.is_fastpath = true;
#endif

    /* Dequeue the destination. */
    endpoint_ptr_set_epQueue_head_np(ep_ptr, TCB_REF(dest->tcbEPNext));
    if (unlikely(dest->tcbEPNext)) {
        dest->tcbEPNext->tcbEPPrev = NULL;
    } else {
        endpoint_ptr_mset_epQueue_tail_state(ep_ptr, 0, EPState_Idle);
    }

    badge = cap_endpoint_cap_get_capEPBadge(ep_cap);

    /* Unlink dest <-> reply, link src (cur thread) <-> reply */
    thread_state_ptr_set_tsType_np(&NODE_STATE(ksCurThread)->tcbState,
                                   ThreadState_BlockedOnReply);
#ifdef CONFIG_KERNEL_MCS
    thread_state_ptr_set_replyObject_np(&dest->tcbState, 0);
    thread_state_ptr_set_replyObject_np(&NODE_STATE(ksCurThread)->tcbState, REPLY_REF(reply));
    reply->replyTCB = NODE_STATE(ksCurThread);

    sched_context_t *sc = NODE_STATE(ksCurThread)->tcbSchedContext;
    sc->scTcb = dest;
    dest->tcbSchedContext = sc;
    NODE_STATE(ksCurThread)->tcbSchedContext = NULL;

    reply_t *old_caller = sc->scReply;
    reply->replyPrev = call_stack_new(REPLY_REF(sc->scReply), false);
    if (unlikely(old_caller)) {
        old_caller->replyNext = call_stack_new(REPLY_REF(reply), false);
    }
    reply->replyNext = call_stack_new(SC_REF(sc), true);
    sc->scReply = reply;
#else
    /* Get sender reply slot */
    cte_t *replySlot = TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbReply);

    /* Get dest caller slot */
    cte_t *callerSlot = TCB_PTR_CTE_PTR(dest, tcbCaller);

    /* Insert reply cap */
    word_t replyCanGrant = thread_state_ptr_get_blockingIPCCanGrant(&dest->tcbState);;
    cap_reply_cap_ptr_new_np(&callerSlot->cap, replyCanGrant, 0,
                             TCB_REF(NODE_STATE(ksCurThread)));
    mdb_node_ptr_set_mdbPrev_np(&callerSlot->cteMDBNode, CTE_REF(replySlot));
    mdb_node_ptr_mset_mdbNext_mdbRevocable_mdbFirstBadged(
        &replySlot->cteMDBNode, CTE_REF(callerSlot), 1, 1);
#endif

    fastpath_copy_mrs(length, NODE_STATE(ksCurThread), dest);

    /* Dest thread is set Running, but not queued. */
    thread_state_ptr_set_tsType_np(&dest->tcbState,
                                   ThreadState_Running);
    switchToThread_fp(dest, cap_pd, stored_hw_asid);

    msgInfo = wordFromMessageInfo(seL4_MessageInfo_set_capsUnwrapped(info, 0));

    fastpath_restore(badge, msgInfo, NODE_STATE(ksCurThread));
}

#ifdef CONFIG_ARCH_ARM
static inline
#ifndef CONFIG_ARCH_ARM_V6
FORCE_INLINE
#endif
#endif
#ifdef CONFIG_KERNEL_MCS
void NORETURN fastpath_reply_recv(word_t cptr, word_t msgInfo, word_t reply)
#else
void NORETURN fastpath_reply_recv(word_t cptr, word_t msgInfo)
#endif
{
    seL4_MessageInfo_t info;
    cap_t ep_cap;
    endpoint_t *ep_ptr;
    word_t length;
    tcb_t *caller;
    word_t badge;
    tcb_t *endpointTail;
    word_t fault_type;

    cap_t newVTable;
    vspace_root_t *cap_pd;
    pde_t stored_hw_asid;
    dom_t dom;

    /* Get message info and length */
    info = messageInfoFromWord_raw(msgInfo);
    length = seL4_MessageInfo_get_length(info);
    fault_type = seL4_Fault_get_seL4_FaultType(NODE_STATE(ksCurThread)->tcbFault);

    /* Check there's no extra caps, the length is ok and there's no
     * saved fault. */
    if (unlikely(fastpath_mi_check(msgInfo) ||
                 fault_type != seL4_Fault_NullFault)) {
        slowpath(SysReplyRecv);
    }

    /* Lookup the cap */
    ep_cap = lookup_fp(TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbCTable)->cap,
                       cptr);

    /* Check it's an endpoint */
    if (unlikely(!cap_capType_equals(ep_cap, cap_endpoint_cap) ||
                 !cap_endpoint_cap_get_capCanReceive(ep_cap))) {
        slowpath(SysReplyRecv);
    }

#ifdef CONFIG_KERNEL_MCS
    /* lookup the reply object */
    cap_t reply_cap = lookup_fp(TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbCTable)->cap, reply);

    /* check it's a reply object */
    if (unlikely(!cap_capType_equals(reply_cap, cap_reply_cap))) {
        slowpath(SysReplyRecv);
    }
#endif

    /* Check there is nothing waiting on the notification */
    if (unlikely(NODE_STATE(ksCurThread)->tcbBoundNotification &&
                 notification_ptr_get_state(NODE_STATE(ksCurThread)->tcbBoundNotification) == NtfnState_Active)) {
        slowpath(SysReplyRecv);
    }

    /* Get the endpoint address */
    ep_ptr = EP_PTR(cap_endpoint_cap_get_capEPPtr(ep_cap));

    /* Check that there's not a thread waiting to send */
    if (unlikely(endpoint_ptr_get_state(ep_ptr) == EPState_Send)) {
        slowpath(SysReplyRecv);
    }

#ifdef CONFIG_KERNEL_MCS
    /* Get the reply address */
    reply_t *reply_ptr = REPLY_PTR(cap_reply_cap_get_capReplyPtr(reply_cap));
    /* check that its valid and at the head of the call chain
       and that the current thread's SC is going to be donated. */
    if (unlikely(reply_ptr->replyTCB == NULL ||
                 call_stack_get_isHead(reply_ptr->replyNext) == 0 ||
                 SC_PTR(call_stack_get_callStackPtr(reply_ptr->replyNext)) != NODE_STATE(ksCurThread)->tcbSchedContext)) {
        slowpath(SysReplyRecv);
    }

    /* Determine who the caller is. */
    caller = reply_ptr->replyTCB;
#else
    /* Only reply if the reply cap is valid. */
    cte_t *callerSlot = TCB_PTR_CTE_PTR(NODE_STATE(ksCurThread), tcbCaller);
    cap_t callerCap = callerSlot->cap;
    if (unlikely(!fastpath_reply_cap_check(callerCap))) {
        slowpath(SysReplyRecv);
    }

    /* Determine who the caller is. */
    caller = TCB_PTR(cap_reply_cap_get_capTCBPtr(callerCap));
#endif

    /* ensure we are not single stepping the caller in ia32 */
#if defined(CONFIG_HARDWARE_DEBUG_API) && defined(CONFIG_ARCH_IA32)
    if (unlikely(caller->tcbArch.tcbContext.breakpointState.single_step_enabled)) {
        slowpath(SysReplyRecv);
    }
#endif

    /* Check that the caller has not faulted, in which case a fault
       reply is generated instead. */
    fault_type = seL4_Fault_get_seL4_FaultType(caller->tcbFault);
    if (unlikely(fault_type != seL4_Fault_NullFault)) {
        slowpath(SysReplyRecv);
    }

    /* Get destination thread.*/
    newVTable = TCB_PTR_CTE_PTR(caller, tcbVTable)->cap;

    /* Get vspace root. */
    cap_pd = cap_vtable_cap_get_vspace_root_fp(newVTable);

    /* Ensure that the destination has a valid MMU. */
    if (unlikely(! isValidVTableRoot_fp(newVTable))) {
        slowpath(SysReplyRecv);
    }

#ifdef CONFIG_ARCH_AARCH32
    /* Get HWASID. */
    stored_hw_asid = cap_pd[PD_ASID_SLOT];
#endif

#ifdef CONFIG_ARCH_X86_64
    stored_hw_asid.words[0] = cap_pml4_cap_get_capPML4MappedASID(newVTable);
#endif
#ifdef CONFIG_ARCH_IA32
    /* stored_hw_asid is unused on ia32 fastpath, but gets passed into a function below. */
    stored_hw_asid.words[0] = 0;
#endif
#ifdef CONFIG_ARCH_AARCH64
    stored_hw_asid.words[0] = cap_vtable_root_get_mappedASID(newVTable);
#endif

#ifdef CONFIG_ARCH_RISCV
    stored_hw_asid.words[0] = cap_page_table_cap_get_capPTMappedASID(newVTable);
#endif

    /* Ensure the original caller can be scheduled directly. */
    dom = maxDom ? ksCurDomain : 0;
    if (unlikely(!isHighestPrio(dom, caller->tcbPriority))) {
        slowpath(SysReplyRecv);
    }

#ifdef CONFIG_ARCH_AARCH32
    /* Ensure the HWASID is valid. */
    if (unlikely(!pde_pde_invalid_get_stored_asid_valid(stored_hw_asid))) {
        slowpath(SysReplyRecv);
    }
#endif

    /* Ensure the original caller is in the current domain and can be scheduled directly. */
    if (unlikely(caller->tcbDomain != ksCurDomain && maxDom)) {
        slowpath(SysReplyRecv);
    }

#ifdef CONFIG_KERNEL_MCS
    if (unlikely(caller->tcbSchedContext != NULL)) {
        slowpath(SysReplyRecv);
    }
#endif

#ifdef ENABLE_SMP_SUPPORT
    /* Ensure both threads have the same affinity */
    if (unlikely(NODE_STATE(ksCurThread)->tcbAffinity != caller->tcbAffinity)) {
        slowpath(SysReplyRecv);
    }
#endif /* ENABLE_SMP_SUPPORT */

#ifdef CONFIG_KERNEL_MCS
    /* not possible to set reply object and not be blocked */
    assert(thread_state_get_replyObject(NODE_STATE(ksCurThread)->tcbState) == 0);
#endif

    /*
     * --- POINT OF NO RETURN ---
     *
     * At this stage, we have committed to performing the IPC.
     */

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
    ksKernelEntry.is_fastpath = true;
#endif

    /* Set thread state to BlockedOnReceive */
    thread_state_ptr_mset_blockingObject_tsType(
        &NODE_STATE(ksCurThread)->tcbState, (word_t)ep_ptr, ThreadState_BlockedOnReceive);
#ifdef CONFIG_KERNEL_MCS
    /* unlink reply object from caller */
    thread_state_ptr_set_replyObject_np(&caller->tcbState, 0);
    /* set the reply object */
    thread_state_ptr_set_replyObject_np(&NODE_STATE(ksCurThread)->tcbState, REPLY_REF(reply_ptr));
    reply_ptr->replyTCB = NODE_STATE(ksCurThread);
#else
    thread_state_ptr_set_blockingIPCCanGrant(&NODE_STATE(ksCurThread)->tcbState,
                                             cap_endpoint_cap_get_capCanGrant(ep_cap));;
#endif

    /* Place the thread in the endpoint queue */
    endpointTail = endpoint_ptr_get_epQueue_tail_fp(ep_ptr);
    if (likely(!endpointTail)) {
        NODE_STATE(ksCurThread)->tcbEPPrev = NULL;
        NODE_STATE(ksCurThread)->tcbEPNext = NULL;

        /* Set head/tail of queue and endpoint state. */
        endpoint_ptr_set_epQueue_head_np(ep_ptr, TCB_REF(NODE_STATE(ksCurThread)));
        endpoint_ptr_mset_epQueue_tail_state(ep_ptr, TCB_REF(NODE_STATE(ksCurThread)),
                                             EPState_Recv);
    } else {
#ifdef CONFIG_KERNEL_MCS
        /* Update queue. */
        tcb_queue_t queue = tcbEPAppend(NODE_STATE(ksCurThread), ep_ptr_get_queue(ep_ptr));
        endpoint_ptr_set_epQueue_head_np(ep_ptr, TCB_REF(queue.head));
        endpoint_ptr_mset_epQueue_tail_state(ep_ptr, TCB_REF(queue.end), EPState_Recv);
#else
        /* Append current thread onto the queue. */
        endpointTail->tcbEPNext = NODE_STATE(ksCurThread);
        NODE_STATE(ksCurThread)->tcbEPPrev = endpointTail;
        NODE_STATE(ksCurThread)->tcbEPNext = NULL;

        /* Update tail of queue. */
        endpoint_ptr_mset_epQueue_tail_state(ep_ptr, TCB_REF(NODE_STATE(ksCurThread)),
                                             EPState_Recv);
#endif
    }

#ifdef CONFIG_KERNEL_MCS
    /* update call stack */
    word_t prev_ptr = call_stack_get_callStackPtr(reply_ptr->replyPrev);
    sched_context_t *sc = NODE_STATE(ksCurThread)->tcbSchedContext;
    NODE_STATE(ksCurThread)->tcbSchedContext = NULL;
    caller->tcbSchedContext = sc;
    sc->scTcb = caller;

    sc->scReply = REPLY_PTR(prev_ptr);
    if (unlikely(REPLY_PTR(prev_ptr) != NULL)) {
        sc->scReply->replyNext = reply_ptr->replyNext;
    }

    /* TODO neccessary? */
    reply_ptr->replyPrev.words[0] = 0;
    reply_ptr->replyNext.words[0] = 0;
#else
    /* Delete the reply cap. */
    mdb_node_ptr_mset_mdbNext_mdbRevocable_mdbFirstBadged(
        &CTE_PTR(mdb_node_get_mdbPrev(callerSlot->cteMDBNode))->cteMDBNode,
        0, 1, 1);
    callerSlot->cap = cap_null_cap_new();
    callerSlot->cteMDBNode = nullMDBNode;
#endif

    /* I know there's no fault, so straight to the transfer. */

    /* Replies don't have a badge. */
    badge = 0;

    fastpath_copy_mrs(length, NODE_STATE(ksCurThread), caller);

    /* Dest thread is set Running, but not queued. */
    thread_state_ptr_set_tsType_np(&caller->tcbState,
                                   ThreadState_Running);
    switchToThread_fp(caller, cap_pd, stored_hw_asid);

    msgInfo = wordFromMessageInfo(seL4_MessageInfo_set_capsUnwrapped(info, 0));

    fastpath_restore(badge, msgInfo, NODE_STATE(ksCurThread));
}
