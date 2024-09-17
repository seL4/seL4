/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <types.h>
#include <string.h>
#include <sel4/constants.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <machine/registerset.h>
#include <model/statedata.h>
#include <object/notification.h>
#include <object/cnode.h>
#include <object/endpoint.h>
#include <object/tcb.h>

#ifdef CONFIG_KERNEL_MCS
void sendIPC(bool_t blocking, bool_t do_call, word_t badge,
             bool_t canGrant, bool_t canGrantReply, bool_t canDonate, tcb_t *thread, endpoint_t *epptr)
#else
void sendIPC(bool_t blocking, bool_t do_call, word_t badge,
             bool_t canGrant, bool_t canGrantReply, tcb_t *thread, endpoint_t *epptr)
#endif
{
    switch (endpoint_ptr_get_state(epptr)) {
    case EPState_Idle:
    case EPState_Send:
        if (blocking) {
            tcb_queue_t queue;

            /* Set thread state to BlockedOnSend */
            thread_state_ptr_set_tsType(&thread->tcbState,
                                        ThreadState_BlockedOnSend);
            thread_state_ptr_set_blockingObject(
                &thread->tcbState, EP_REF(epptr));
            thread_state_ptr_set_blockingIPCBadge(
                &thread->tcbState, badge);
            thread_state_ptr_set_blockingIPCCanGrant(
                &thread->tcbState, canGrant);
            thread_state_ptr_set_blockingIPCCanGrantReply(
                &thread->tcbState, canGrantReply);
            thread_state_ptr_set_blockingIPCIsCall(
                &thread->tcbState, do_call);

            scheduleTCB(thread);

            /* Place calling thread in endpoint queue */
            queue = ep_ptr_get_queue(epptr);
            queue = tcbEPAppend(thread, queue);
            endpoint_ptr_set_state(epptr, EPState_Send);
            ep_ptr_set_queue(epptr, queue);
        }
        break;

    case EPState_Recv: {
        tcb_queue_t queue;
        tcb_t *dest;

        /* Get the head of the endpoint queue. */
        queue = ep_ptr_get_queue(epptr);
        dest = queue.head;

        /* Haskell error "Receive endpoint queue must not be empty" */
        assert(dest);

        /* Dequeue the first TCB */
        queue = tcbEPDequeue(dest, queue);
        ep_ptr_set_queue(epptr, queue);

        if (!queue.head) {
            endpoint_ptr_set_state(epptr, EPState_Idle);
        }

        /* Do the transfer */
        doIPCTransfer(thread, epptr, badge, canGrant, dest);

#ifdef CONFIG_KERNEL_MCS
        reply_t *reply = REPLY_PTR(thread_state_get_replyObject(dest->tcbState));
        if (reply) {
            reply_unlink(reply, dest);
        }

        if (do_call ||
            seL4_Fault_ptr_get_seL4_FaultType(&thread->tcbFault) != seL4_Fault_NullFault) {
            if (reply != NULL && (canGrant || canGrantReply)) {
                reply_push(thread, dest, reply, canDonate);
            } else {
                setThreadState(thread, ThreadState_Inactive);
            }
        } else if (canDonate && dest->tcbSchedContext == NULL) {
            schedContext_donate(thread->tcbSchedContext, dest);
        }

        /* blocked threads should have enough budget to get out of the kernel */
        assert(dest->tcbSchedContext == NULL || refill_sufficient(dest->tcbSchedContext, 0));
        assert(dest->tcbSchedContext == NULL || refill_ready(dest->tcbSchedContext));
        setThreadState(dest, ThreadState_Running);
        if (sc_sporadic(dest->tcbSchedContext) && dest->tcbSchedContext != NODE_STATE(ksCurSC)) {
            refill_unblock_check(dest->tcbSchedContext);
        }
        possibleSwitchTo(dest);
#else
        bool_t replyCanGrant = thread_state_ptr_get_blockingIPCCanGrant(&dest->tcbState);;

        setThreadState(dest, ThreadState_Running);
        possibleSwitchTo(dest);

        if (do_call) {
            if (canGrant || canGrantReply) {
                setupCallerCap(thread, dest, replyCanGrant);
            } else {
                setThreadState(thread, ThreadState_Inactive);
            }
        }
#endif
        break;
    }
    }
}

#ifdef CONFIG_KERNEL_MCS
void receiveIPC(tcb_t *thread, cap_t cap, bool_t isBlocking, cap_t replyCap)
#else
void receiveIPC(tcb_t *thread, cap_t cap, bool_t isBlocking)
#endif
{
    endpoint_t *epptr;
    notification_t *ntfnPtr;

    /* Haskell error "receiveIPC: invalid cap" */
    assert(cap_get_capType(cap) == cap_endpoint_cap);

    epptr = EP_PTR(cap_endpoint_cap_get_capEPPtr(cap));

#ifdef CONFIG_KERNEL_MCS
    reply_t *replyPtr = NULL;
    if (cap_get_capType(replyCap) == cap_reply_cap) {
        replyPtr = REPLY_PTR(cap_reply_cap_get_capReplyPtr(replyCap));
        if (unlikely(replyPtr->replyTCB != NULL && replyPtr->replyTCB != thread)) {
            userError("Reply object already has unexecuted reply!");
            cancelIPC(replyPtr->replyTCB);
        }
    }
#endif

    /* Check for anything waiting in the notification */
    ntfnPtr = thread->tcbBoundNotification;
    if (ntfnPtr && notification_ptr_get_state(ntfnPtr) == NtfnState_Active) {
        completeSignal(ntfnPtr, thread);
    } else {
#ifdef CONFIG_KERNEL_MCS
        /* If this is a blocking recv and we didn't have a pending notification,
         * then if we are running on an SC from a bound notification, then we
         * need to return it so that we can passively wait on the EP for potentially
         * SC donations from client threads.
         */
        if (ntfnPtr && isBlocking) {
            maybeReturnSchedContext(ntfnPtr, thread);
        }
#endif
        switch (endpoint_ptr_get_state(epptr)) {
        case EPState_Idle:
        case EPState_Recv: {
            tcb_queue_t queue;

            if (isBlocking) {
                /* Set thread state to BlockedOnReceive */
                thread_state_ptr_set_tsType(&thread->tcbState,
                                            ThreadState_BlockedOnReceive);
                thread_state_ptr_set_blockingObject(
                    &thread->tcbState, EP_REF(epptr));
#ifdef CONFIG_KERNEL_MCS
                thread_state_ptr_set_replyObject(&thread->tcbState, REPLY_REF(replyPtr));
                if (replyPtr) {
                    replyPtr->replyTCB = thread;
                }
#else
                thread_state_ptr_set_blockingIPCCanGrant(
                    &thread->tcbState, cap_endpoint_cap_get_capCanGrant(cap));
#endif
                scheduleTCB(thread);

                /* Place calling thread in endpoint queue */
                queue = ep_ptr_get_queue(epptr);
                queue = tcbEPAppend(thread, queue);
                endpoint_ptr_set_state(epptr, EPState_Recv);
                ep_ptr_set_queue(epptr, queue);
            } else {
                doNBRecvFailedTransfer(thread);
            }
            break;
        }

        case EPState_Send: {
            tcb_queue_t queue;
            tcb_t *sender;
            word_t badge;
            bool_t canGrant;
            bool_t canGrantReply;
            bool_t do_call;

            /* Get the head of the endpoint queue. */
            queue = ep_ptr_get_queue(epptr);
            sender = queue.head;

            /* Haskell error "Send endpoint queue must not be empty" */
            assert(sender);

            /* Dequeue the first TCB */
            queue = tcbEPDequeue(sender, queue);
            ep_ptr_set_queue(epptr, queue);

            if (!queue.head) {
                endpoint_ptr_set_state(epptr, EPState_Idle);
            }

            /* Get sender IPC details */
            badge = thread_state_ptr_get_blockingIPCBadge(&sender->tcbState);
            canGrant =
                thread_state_ptr_get_blockingIPCCanGrant(&sender->tcbState);
            canGrantReply =
                thread_state_ptr_get_blockingIPCCanGrantReply(&sender->tcbState);

            /* Do the transfer */
            doIPCTransfer(sender, epptr, badge,
                          canGrant, thread);

            do_call = thread_state_ptr_get_blockingIPCIsCall(&sender->tcbState);

#ifdef CONFIG_KERNEL_MCS
            if (sc_sporadic(sender->tcbSchedContext)) {
                /* We know that the sender can't have the current SC as
                 * its own SC as this point as it should still be
                 * associated with the current thread, no thread, or a
                 * thread that isn't blocked. This check is added here
                 * to reduce the cost of proving this to be true as a
                 * short-term stop-gap. */
                assert(sender->tcbSchedContext != NODE_STATE(ksCurSC));
                if (sender->tcbSchedContext != NODE_STATE(ksCurSC)) {
                    refill_unblock_check(sender->tcbSchedContext);
                }
            }

            if (do_call ||
                seL4_Fault_get_seL4_FaultType(sender->tcbFault) != seL4_Fault_NullFault) {
                if ((canGrant || canGrantReply) && replyPtr != NULL) {
                    bool_t canDonate = sender->tcbSchedContext != NULL
                                       && seL4_Fault_get_seL4_FaultType(sender->tcbFault) != seL4_Fault_Timeout;
                    reply_push(sender, thread, replyPtr, canDonate);
                } else {
                    setThreadState(sender, ThreadState_Inactive);
                }
            } else {
                setThreadState(sender, ThreadState_Running);
                possibleSwitchTo(sender);
                assert(sender->tcbSchedContext == NULL || refill_sufficient(sender->tcbSchedContext, 0));
            }
#else
            if (do_call) {
                if (canGrant || canGrantReply) {
                    setupCallerCap(sender, thread, cap_endpoint_cap_get_capCanGrant(cap));
                } else {
                    setThreadState(sender, ThreadState_Inactive);
                }
            } else {
                setThreadState(sender, ThreadState_Running);
                possibleSwitchTo(sender);
            }
#endif
            break;
        }
        }
    }
}

void replyFromKernel_error(tcb_t *thread)
{
    word_t len;
    register_t *ipcBuffer;

    ipcBuffer = lookupIPCBuffer(true, thread);
    setRegister(thread, badgeRegister, 0);
    len = setMRs_syscall_error(thread, ipcBuffer);

#ifdef CONFIG_KERNEL_INVOCATION_REPORT_ERROR_IPC
    char *debugBuffer = (char *)(ipcBuffer + DEBUG_MESSAGE_START + 1);
    word_t add = strlcpy(debugBuffer, (char *)current_debug_error.errorMessage,
                         DEBUG_MESSAGE_MAXLEN * sizeof(word_t));

    len += (add / sizeof(word_t)) + 1;
#endif

    setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                    seL4_MessageInfo_new(current_syscall_error.type, 0, 0, len)));
}

void replyFromKernel_success_empty(tcb_t *thread)
{
    setRegister(thread, badgeRegister, 0);
    setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                    seL4_MessageInfo_new(0, 0, 0, 0)));
}

void cancelIPC(tcb_t *tptr)
{
    thread_state_t *state = &tptr->tcbState;

#ifdef CONFIG_KERNEL_MCS
    /* cancel ipc cancels all faults */
    seL4_Fault_NullFault_ptr_new(&tptr->tcbFault);
#endif

    switch (thread_state_ptr_get_tsType(state)) {
    case ThreadState_BlockedOnSend:
    case ThreadState_BlockedOnReceive: {
        /* blockedIPCCancel state */
        endpoint_t *epptr;
        tcb_queue_t queue;

        epptr = EP_PTR(thread_state_ptr_get_blockingObject(state));

        /* Haskell error "blockedIPCCancel: endpoint must not be idle" */
        assert(endpoint_ptr_get_state(epptr) != EPState_Idle);

        /* Dequeue TCB */
        queue = ep_ptr_get_queue(epptr);
        queue = tcbEPDequeue(tptr, queue);
        ep_ptr_set_queue(epptr, queue);

        if (!queue.head) {
            endpoint_ptr_set_state(epptr, EPState_Idle);
        }

#ifdef CONFIG_KERNEL_MCS
        reply_t *reply = REPLY_PTR(thread_state_get_replyObject(tptr->tcbState));
        if (reply != NULL) {
            reply_unlink(reply, tptr);
        }
#endif
        setThreadState(tptr, ThreadState_Inactive);
        break;
    }

    case ThreadState_BlockedOnNotification:
        cancelSignal(tptr,
                     NTFN_PTR(thread_state_ptr_get_blockingObject(state)));
        break;

    case ThreadState_BlockedOnReply: {
#ifdef CONFIG_KERNEL_MCS
        reply_remove_tcb(tptr);
#else
        cte_t *slot, *callerCap;

        tptr->tcbFault = seL4_Fault_NullFault_new();

        /* Get the reply cap slot */
        slot = TCB_PTR_CTE_PTR(tptr, tcbReply);

        callerCap = CTE_PTR(mdb_node_get_mdbNext(slot->cteMDBNode));
        if (callerCap) {
            /** GHOSTUPD: "(True,
                gs_set_assn cteDeleteOne_'proc (ucast cap_reply_cap))" */
            cteDeleteOne(callerCap);
        }
#endif

        break;
    }
    }
}

void cancelAllIPC(endpoint_t *epptr)
{
    switch (endpoint_ptr_get_state(epptr)) {
    case EPState_Idle:
        break;

    default: {
        tcb_t *thread = TCB_PTR(endpoint_ptr_get_epQueue_head(epptr));

        /* Make endpoint idle */
        endpoint_ptr_set_state(epptr, EPState_Idle);
        endpoint_ptr_set_epQueue_head(epptr, 0);
        endpoint_ptr_set_epQueue_tail(epptr, 0);

        /* Set all blocked threads to restart */
        for (; thread; thread = thread->tcbEPNext) {
#ifdef CONFIG_KERNEL_MCS
            reply_t *reply = REPLY_PTR(thread_state_get_replyObject(thread->tcbState));
            if (reply != NULL) {
                reply_unlink(reply, thread);
            }
            if (seL4_Fault_get_seL4_FaultType(thread->tcbFault) == seL4_Fault_NullFault) {
                setThreadState(thread, ThreadState_Restart);
                if (sc_sporadic(thread->tcbSchedContext)) {
                    /* We know that the thread can't have the current SC
                     * as its own SC as this point as it should still be
                     * associated with the current thread, or no thread.
                     * This check is added here to reduce the cost of
                     * proving this to be true as a short-term stop-gap. */
                    assert(thread->tcbSchedContext != NODE_STATE(ksCurSC));
                    if (thread->tcbSchedContext != NODE_STATE(ksCurSC)) {
                        refill_unblock_check(thread->tcbSchedContext);
                    }
                }
                possibleSwitchTo(thread);
            } else {
                setThreadState(thread, ThreadState_Inactive);
            }
#else
            setThreadState(thread, ThreadState_Restart);
            SCHED_ENQUEUE(thread);
#endif
        }

        rescheduleRequired();
        break;
    }
    }
}

void cancelBadgedSends(endpoint_t *epptr, word_t badge)
{
    switch (endpoint_ptr_get_state(epptr)) {
    case EPState_Idle:
    case EPState_Recv:
        break;

    case EPState_Send: {
        tcb_t *thread, *next;
        tcb_queue_t queue = ep_ptr_get_queue(epptr);

        /* this is a de-optimisation for verification
         * reasons. it allows the contents of the endpoint
         * queue to be ignored during the for loop. */
        endpoint_ptr_set_state(epptr, EPState_Idle);
        endpoint_ptr_set_epQueue_head(epptr, 0);
        endpoint_ptr_set_epQueue_tail(epptr, 0);

        for (thread = queue.head; thread; thread = next) {
            word_t b = thread_state_ptr_get_blockingIPCBadge(
                           &thread->tcbState);
            next = thread->tcbEPNext;
#ifdef CONFIG_KERNEL_MCS
            /* senders do not have reply objects in their state, and we are only cancelling sends */
            assert(REPLY_PTR(thread_state_get_replyObject(thread->tcbState)) == NULL);
            if (b == badge) {
                if (seL4_Fault_get_seL4_FaultType(thread->tcbFault) ==
                    seL4_Fault_NullFault) {
                    setThreadState(thread, ThreadState_Restart);
                    if (sc_sporadic(thread->tcbSchedContext)) {
                        /* We know that the thread can't have the current SC
                         * as its own SC as this point as it should still be
                         * associated with the current thread, or no thread.
                         * This check is added here to reduce the cost of
                         * proving this to be true as a short-term stop-gap. */
                        assert(thread->tcbSchedContext != NODE_STATE(ksCurSC));
                        if (thread->tcbSchedContext != NODE_STATE(ksCurSC)) {
                            refill_unblock_check(thread->tcbSchedContext);
                        }
                    }
                    possibleSwitchTo(thread);
                } else {
                    setThreadState(thread, ThreadState_Inactive);
                }
                queue = tcbEPDequeue(thread, queue);
            }
#else
            if (b == badge) {
                setThreadState(thread, ThreadState_Restart);
                SCHED_ENQUEUE(thread);
                queue = tcbEPDequeue(thread, queue);
            }
#endif
        }
        ep_ptr_set_queue(epptr, queue);

        if (queue.head) {
            endpoint_ptr_set_state(epptr, EPState_Send);
        }

        rescheduleRequired();

        break;
    }

    default:
        fail("invalid EP state");
    }
}

#ifdef CONFIG_KERNEL_MCS
void reorderEP(endpoint_t *epptr, tcb_t *thread)
{
    tcb_queue_t queue = ep_ptr_get_queue(epptr);
    queue = tcbEPDequeue(thread, queue);
    queue = tcbEPAppend(thread, queue);
    ep_ptr_set_queue(epptr, queue);
}
#endif
