/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <object/reply.h>

void reply_push(tcb_t *tcb_caller, tcb_t *tcb_callee, reply_t *reply, bool_t canDonate)
{
    sched_context_t *sc_donated = tcb_caller->tcbSchedContext;

    assert(tcb_caller != NULL);
    assert(reply != NULL);
    assert(reply->replyTCB == NULL);

    assert(call_stack_get_callStackPtr(reply->replyPrev) == 0);
    assert(call_stack_get_callStackPtr(reply->replyNext) == 0);

    /* caller state must be blocked on send or active, so cannot be in a existing call stack */
    assert(thread_state_get_tsType(tcb_caller->tcbState) == ThreadState_BlockedOnSend ||
           thread_state_get_tsType(tcb_caller->tcbState) == ThreadState_Running ||
           thread_state_get_tsType(tcb_caller->tcbState) == ThreadState_Restart);

    /* callee state must be simple at this point (inactive, restart, or running),
       so it also cannot have a reply reference associated with it. */
    assert(thread_state_get_tsType(tcb_callee->tcbState) == ThreadState_Inactive ||
           thread_state_get_tsType(tcb_callee->tcbState) == ThreadState_Running ||
           thread_state_get_tsType(tcb_callee->tcbState) == ThreadState_Restart);

    /* link caller and reply */
    reply->replyTCB = tcb_caller;
    setThreadStateBlockedOnReply(tcb_caller, reply);

    if (sc_donated != NULL && tcb_callee->tcbSchedContext == NULL && canDonate) {
        reply_t *old_caller = sc_donated->scReply;

        /* check stack integrity */
        assert(old_caller == NULL ||
               SC_PTR(call_stack_get_callStackPtr(old_caller->replyNext)) == sc_donated);

        /* push on to stack */
        if (old_caller) {
            old_caller->replyNext = call_stack_new(REPLY_REF(reply), false);
        }
        reply->replyPrev = call_stack_new(REPLY_REF(old_caller), false);
        sc_donated->scReply = reply;
        reply->replyNext = call_stack_new(SC_REF(sc_donated), true);

        /* now do the actual donation */
        schedContext_donate(sc_donated, tcb_callee);
    }
}

/* Pop the head reply from the call stack */
void reply_pop(reply_t *reply, tcb_t *tcb)
{
    assert(reply != NULL);
    assert(reply->replyTCB == tcb);
    assert(thread_state_get_tsType(tcb->tcbState) == ThreadState_BlockedOnReply);
    assert(thread_state_get_replyObject(tcb->tcbState) == REPLY_REF(reply));

    word_t next_ptr = call_stack_get_callStackPtr(reply->replyNext);
    word_t prev_ptr = call_stack_get_callStackPtr(reply->replyPrev);

    SC_PTR(next_ptr)->scReply = REPLY_PTR(prev_ptr);
    if (prev_ptr != 0) {
        REPLY_PTR(prev_ptr)->replyNext = reply->replyNext;
        assert(call_stack_get_isHead(REPLY_PTR(prev_ptr)->replyNext));
    }

    reply->replyNext = call_stack_new(0, false);

    /* give it back */
    if (tcb->tcbSchedContext == NULL) {
        /* only give the SC back if our SC is NULL. This prevents
         * strange behaviour when a thread is bound to an sc while it is
         * in the BlockedOnReply state. The semantics in this case are that the
         * SC cannot go back to the caller if the caller has received another one */
        schedContext_donate(SC_PTR(next_ptr), tcb);
    }

    reply->replyPrev = call_stack_new(0, false);
    reply_unlink(reply, tcb);
}

/* Remove a reply from the middle of the call stack */
void reply_remove(reply_t *reply, tcb_t *tcb)
{
    assert(reply->replyTCB == tcb);
    assert(thread_state_get_tsType(tcb->tcbState) == ThreadState_BlockedOnReply);
    assert(thread_state_get_replyObject(tcb->tcbState) == REPLY_REF(reply));

    word_t next_ptr = call_stack_get_callStackPtr(reply->replyNext);
    word_t prev_ptr = call_stack_get_callStackPtr(reply->replyPrev);

    if (likely(next_ptr && call_stack_get_isHead(reply->replyNext))) {
        /* head of the call stack -> just pop */
        reply_pop(reply, tcb);
    } else {
        if (next_ptr) {
            /* not the head, remove from middle - break the chain */
            REPLY_PTR(next_ptr)->replyPrev = call_stack_new(0, false);
        }
        if (prev_ptr) {
            REPLY_PTR(prev_ptr)->replyNext = call_stack_new(0, false);
        }
        reply->replyPrev = call_stack_new(0, false);
        reply->replyNext = call_stack_new(0, false);
        reply_unlink(reply, tcb);
    }
}

void reply_remove_tcb(tcb_t *tcb)
{
    assert(thread_state_get_tsType(tcb->tcbState) == ThreadState_BlockedOnReply);
    reply_t *reply = REPLY_PTR(thread_state_get_replyObject(tcb->tcbState));
    word_t next_ptr = call_stack_get_callStackPtr(reply->replyNext);
    word_t prev_ptr = call_stack_get_callStackPtr(reply->replyPrev);

    if (next_ptr) {
        if (call_stack_get_isHead(reply->replyNext)) {
            SC_PTR(next_ptr)->scReply = NULL;
        } else {
            REPLY_PTR(next_ptr)->replyPrev = call_stack_new(0, false);
        }
    }

    if (prev_ptr) {
        REPLY_PTR(prev_ptr)->replyNext = call_stack_new(0, false);
    }

    reply->replyPrev = call_stack_new(0, false);
    reply->replyNext = call_stack_new(0, false);
    reply_unlink(reply, tcb);
}
