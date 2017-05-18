/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */
#include <object/reply.h>

void
reply_push(tcb_t *tcb_caller, tcb_t *tcb_callee, reply_t *reply, bool_t canDonate) {
    sched_context_t *sc_donated = tcb_caller->tcbSchedContext;

    assert(tcb_caller != NULL);
    assert(reply != NULL);

    if (unlikely(reply->replyCaller)) {
        /* this reply should not be in an existing call stack - but it is
         * this could be intentional but warn the user in debug mode anyway */
        userError("Reply object already has unexecuted reply! Invoke first.");
        reply_remove(reply);
    }

    if (tcb_callee->tcbSchedContext) {
        /* receiver already has sc */
        canDonate = false;
    }

    assert(call_stack_get_callStackPtr(reply->replyPrev) == 0);
    assert(call_stack_get_callStackPtr(reply->replyNext) == 0);
    assert(reply->replyCaller == NULL);

    /* tcb caller should not be in a existing call stack */
    assert(tcb_caller->tcbReply == NULL);

    /* link tcb and reply */
    reply->replyCaller = tcb_caller;
    tcb_caller->tcbReply = reply;

    if (sc_donated != NULL && canDonate) {
        assert(tcb_callee->tcbSchedContext == NULL);

        reply_t *old_caller = sc_donated->scReply;

        /* check stack integrity */
        assert(old_caller == NULL ||
               SC_PTR(call_stack_get_callStackPtr(old_caller->replyNext)) == sc_donated);

        /* push on to stack */
        reply->replyPrev = call_stack_new(REPLY_REF(old_caller), false);
        if (old_caller) {
            old_caller->replyNext = call_stack_new(REPLY_REF(reply), false);
        }
        reply->replyNext = call_stack_new(SC_REF(sc_donated), true);
        sc_donated->scReply = reply;

        /* now do the actual donation */
        schedContext_donate(sc_donated, tcb_callee);
    }
}

/* Pop the head reply from the call stack */
void
reply_pop(reply_t *reply)
{
    assert(reply != NULL);
    assert(reply->replyCaller != NULL);

    /* unlink tcb and reply */
    tcb_t *tcb = reply->replyCaller;
    reply->replyCaller = NULL;
    tcb->tcbReply = NULL;

    word_t next_ptr = call_stack_get_callStackPtr(reply->replyNext);
    word_t prev_ptr = call_stack_get_callStackPtr(reply->replyPrev);

    if (likely(next_ptr != 0)) {
        assert(call_stack_get_isHead(reply->replyNext));

        /* give it back */
        schedContext_donate(SC_PTR(next_ptr), tcb);

        SC_PTR(next_ptr)->scReply = REPLY_PTR(prev_ptr);
        if (prev_ptr != 0) {
            REPLY_PTR(prev_ptr)->replyNext = reply->replyNext;
        }

        call_stack_ptr_set_callStackPtr(&reply->replyPrev, 0);
        call_stack_ptr_set_callStackPtr(&reply->replyNext, 0);
    }
}

/* Remove a reply from the middle of the call stack */
void
reply_remove(reply_t *reply)
{
    word_t next_ptr = call_stack_get_callStackPtr(reply->replyNext);
    word_t prev_ptr = call_stack_get_callStackPtr(reply->replyPrev);

    if (likely(next_ptr)) {
        if (likely(call_stack_get_isHead(reply->replyNext))) {
            /* head of the call stack -> just pop */
            reply_pop(reply);
            return;
        }
        /* not the head, remove from middle */
        REPLY_PTR(next_ptr)->replyPrev = reply->replyPrev;
        REPLY_PTR(next_ptr)->replyCaller = reply->replyCaller;
        if (reply->replyCaller) {
            /* to maintain the call chain, we remove this caller and
             * replaced them with the next */
            reply->replyCaller->tcbReply = REPLY_PTR(next_ptr);
            reply->replyCaller = NULL;
        }
    } else if (reply->replyCaller) {
        /* removing start of call chain */
        reply->replyCaller->tcbReply = NULL;
        reply->replyCaller = NULL;
    }

    if (prev_ptr) {
        REPLY_PTR(prev_ptr)->replyNext = reply->replyNext;
    }

    call_stack_ptr_set_callStackPtr(&reply->replyPrev, 0);
    call_stack_ptr_set_callStackPtr(&reply->replyNext, 0);
}
