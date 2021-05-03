/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef CONFIG_KERNEL_MCS
#include <object/reply.h>
#include <object/notification.h>
#endif

/* Fastpath cap lookup.  Returns a null_cap on failure. */
static inline cap_t FORCE_INLINE lookup_fp(cap_t cap, cptr_t cptr)
{
    word_t cptr2;
    cte_t *slot;
    word_t guardBits, radixBits, bits;
    word_t radix, capGuard;

    bits = 0;

    if (unlikely(! cap_capType_equals(cap, cap_cnode_cap))) {
        return cap_null_cap_new();
    }

    do {
        guardBits = cap_cnode_cap_get_capCNodeGuardSize(cap);
        radixBits = cap_cnode_cap_get_capCNodeRadix(cap);
        cptr2 = cptr << bits;

        capGuard = cap_cnode_cap_get_capCNodeGuard(cap);

        /* Check the guard. Depth mismatch check is deferred.
           The 32MinusGuardSize encoding contains an exception
           when the guard is 0, when 32MinusGuardSize will be
           reported as 0 also. In this case we skip the check */
        if (likely(guardBits) && unlikely(cptr2 >> (wordBits - guardBits) != capGuard)) {
            return cap_null_cap_new();
        }

        radix = cptr2 << guardBits >> (wordBits - radixBits);
        slot = CTE_PTR(cap_cnode_cap_get_capCNodePtr(cap)) + radix;

        cap = slot->cap;
        bits += guardBits + radixBits;

    } while (unlikely(bits < wordBits && cap_capType_equals(cap, cap_cnode_cap)));

    if (unlikely(bits > wordBits)) {
        /* Depth mismatch. We've overshot wordBits bits. The lookup we've done is
           safe, but wouldn't be allowed by the slowpath. */
        return cap_null_cap_new();
    }

    return cap;
}
/* make sure the fastpath functions conform with structure_*.bf */
static inline void thread_state_ptr_set_tsType_np(thread_state_t *ts_ptr, word_t tsType)
{
    ts_ptr->words[0] = tsType;
}

static inline void thread_state_ptr_mset_blockingObject_tsType(thread_state_t *ts_ptr,
                                                               word_t ep_ref,
                                                               word_t tsType)
{
    ts_ptr->words[0] = ep_ref | tsType;
}

#ifndef CONFIG_KERNEL_MCS
static inline void cap_reply_cap_ptr_new_np(cap_t *cap_ptr, word_t capReplyCanGrant,
                                            word_t capReplyMaster, word_t capTCBPtr)
{
#ifdef __KERNEL_64__
    cap_ptr->words[1] = (word_t)capTCBPtr;
    cap_ptr->words[0] = (capReplyMaster) | (capReplyCanGrant << 1) |
                        ((word_t)cap_reply_cap << 59);
#else
    cap_ptr->words[0] = TCB_REF(capTCBPtr) | (capReplyMaster << 4) |
                        (capReplyCanGrant << 5) | cap_reply_cap ;
#endif
}
#endif

static inline void endpoint_ptr_mset_epQueue_tail_state(endpoint_t *ep_ptr, word_t epQueue_tail,
                                                        word_t state)
{
    ep_ptr->words[0] = epQueue_tail | state;
}

static inline void endpoint_ptr_set_epQueue_head_np(endpoint_t *ep_ptr, word_t epQueue_head)
{
    ep_ptr->words[1] = epQueue_head;
}

#ifdef CONFIG_KERNEL_MCS
static inline void thread_state_ptr_set_replyObject_np(thread_state_t *ts_ptr, word_t reply)
{
    assert(!thread_state_ptr_get_tcbQueued(ts_ptr));
    assert(!thread_state_ptr_get_tcbInReleaseQueue(ts_ptr));
#if CONFIG_WORD_SIZE == 64
    thread_state_ptr_set_replyObject(ts_ptr, REPLY_REF(reply));
#else
    ts_ptr->words[1] = REPLY_REF(reply);
#endif
}

static inline reply_t *thread_state_get_replyObject_np(thread_state_t ts)
{
    assert(!thread_state_get_tcbQueued(ts));
    assert(!thread_state_get_tcbInReleaseQueue(ts));
#if CONFIG_WORD_SIZE == 64
    return REPLY_PTR(thread_state_get_replyObject(ts));
#else
    return REPLY_PTR(ts.words[1]);
#endif
}
#endif

static inline void cancelSignal_fp(tcb_t *tptr, notification_t *ntfnPtr)
{
    tcb_queue_t ntfn_queue;

    /* Haskell error "cancelSignal: notification object must be in a waiting" state */
    assert(notification_ptr_get_state(ntfnPtr) == NtfnState_Waiting);

    ntfn_queue.head = (tcb_t *)notification_ptr_get_ntfnQueue_head(ntfnPtr);
    ntfn_queue.end = (tcb_t *)notification_ptr_get_ntfnQueue_tail(ntfnPtr);

    /* Dequeue TCB */
    ntfn_queue = tcbEPDequeue(tptr, ntfn_queue);
    notification_ptr_set_ntfnQueue_head(ntfnPtr, (word_t)ntfn_queue.head);
    notification_ptr_set_ntfnQueue_tail(ntfnPtr, (word_t)ntfn_queue.end);

    /* set the thread state to idle if the queue is empty */
    if (!ntfn_queue.head) {
        notification_ptr_set_state(ntfnPtr, NtfnState_Idle);
    }

    /* Make thread inactive */
    thread_state_ptr_set_tsType_np(&tptr->tcbState, ThreadState_Inactive);
}

static inline void cancelIPC_fp(tcb_t *tptr)
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

        epptr = EP_PTR(thread_state_ptr_get_blockingObject(state));

        /* Haskell error "blockedIPCCancel: endpoint must not be idle" */
        assert(endpoint_ptr_get_state(epptr) != EPState_Idle);

        /* Dequeue TCB */
        endpoint_ptr_set_epQueue_head_np(epptr, TCB_REF(tptr->tcbEPNext));
        if (unlikely(tptr->tcbEPNext)) {
            tptr->tcbEPNext->tcbEPPrev = NULL;
        } else {
            endpoint_ptr_mset_epQueue_tail_state(epptr, 0, EPState_Idle);
        }

#ifdef CONFIG_KERNEL_MCS
        reply_t *reply = REPLY_PTR(thread_state_get_replyObject(tptr->tcbState));
        if (reply != NULL) {
            reply_unlink(reply, tptr);
        }
#endif
        thread_state_ptr_set_tsType_np(&tptr->tcbState, ThreadState_Inactive);
        break;
    }

    case ThreadState_BlockedOnNotification:
        cancelSignal_fp(tptr,
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

#include <arch/fastpath/fastpath.h>

