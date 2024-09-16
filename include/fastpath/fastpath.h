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

#ifdef CONFIG_SIGNAL_FASTPATH
/* Equivalent to schedContext_donate without migrateTCB() */
static inline void maybeDonateSchedContext_fp(tcb_t *dest, sched_context_t *sc)
{
    if (!dest->tcbSchedContext) {
        sc->scTcb = dest;
        dest->tcbSchedContext = sc;
    }

#ifdef ENABLE_SMP_SUPPORT
#ifdef CONFIG_DEBUG_BUILD
    tcbDebugRemove(dest);
#endif
    /* The part of migrateTCB() that doesn't involve the slowpathed FPU save */
    dest->tcbAffinity = sc->scCore;
#ifdef CONFIG_DEBUG_BUILD
    tcbDebugAppend(dest);
#endif
#endif
}

static inline void cancelIPC_fp(tcb_t *dest)
{
    endpoint_t *ep_ptr;
    tcb_queue_t queue;
    ep_ptr = EP_PTR(thread_state_get_blockingObject(dest->tcbState));

    queue = ep_ptr_get_queue(ep_ptr);
    queue = tcbEPDequeue(dest, queue);
    ep_ptr_set_queue(ep_ptr, queue);

    if (!queue.head) {
        endpoint_ptr_set_state(ep_ptr, EPState_Idle);
    }

    reply_t *reply = REPLY_PTR(thread_state_get_replyObject(dest->tcbState));
    if (reply != NULL) {
        reply_unlink(reply, dest);
    }
}

/* Dequeue TCB from notification queue */
static inline void ntfn_queue_dequeue_fp(tcb_t *dest, notification_t *ntfn_ptr)
{
    tcb_queue_t ntfn_queue;
    ntfn_queue.head = (tcb_t *)notification_ptr_get_ntfnQueue_head(ntfn_ptr);
    ntfn_queue.end = (tcb_t *)notification_ptr_get_ntfnQueue_tail(ntfn_ptr);

    ntfn_queue = tcbEPDequeue(dest, ntfn_queue);

    notification_ptr_set_ntfnQueue_head(ntfn_ptr, (word_t)ntfn_queue.head);
    notification_ptr_set_ntfnQueue_tail(ntfn_ptr, (word_t)ntfn_queue.end);

    if (!ntfn_queue.head) {
        notification_ptr_set_state(ntfn_ptr, NtfnState_Idle);
    }
}
#endif

#ifdef CONFIG_EXCEPTION_FASTPATH
static inline void fastpath_vm_fault_set_mrs(tcb_t *dest)
{
    setRegister(dest, msgRegisters[0] + seL4_VMFault_IP, getRestartPC(NODE_STATE(ksCurThread)));
    setRegister(dest, msgRegisters[0] + seL4_VMFault_Addr,
                seL4_Fault_VMFault_get_address(NODE_STATE(ksCurThread)->tcbFault));
    setRegister(dest, msgRegisters[0] + seL4_VMFault_PrefetchFault,
                seL4_Fault_VMFault_get_instructionFault(NODE_STATE(ksCurThread)->tcbFault));
    setRegister(dest, msgRegisters[0] + seL4_VMFault_FSR,
                seL4_Fault_VMFault_get_FSR(NODE_STATE(ksCurThread)->tcbFault));
}
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
                                                               pptr_t ep_ref,
                                                               word_t tsType)
{
    ts_ptr->words[0] = ep_ref | tsType;
}

#ifndef CONFIG_KERNEL_MCS
static inline void cap_reply_cap_ptr_new_np(cap_t *cap_ptr, word_t capReplyCanGrant,
                                            pptr_t capReplyMaster, pptr_t capTCBPtr)
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

static inline void endpoint_ptr_mset_epQueue_tail_state(endpoint_t *ep_ptr, pptr_t epQueue_tail,
                                                        word_t state)
{
    ep_ptr->words[0] = epQueue_tail | state;
}

static inline void endpoint_ptr_set_epQueue_head_np(endpoint_t *ep_ptr, pptr_t epQueue_head)
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

#include <arch/fastpath/fastpath.h>

