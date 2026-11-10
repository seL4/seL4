/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <object/reply.h>

void reply_link_caller(tcb_t *tcb_caller, tcb_t *tcb_callee, reply_t *reply, bool_t canDonate)
{
    sched_context_t *sc_donated = tcb_caller->tcbSchedContext;

    assert(tcb_caller != NULL);
    assert(reply != NULL);
    assert(reply->replyTCB == NULL);

    assert(reply->replyPrev == NULL);
    assert(reply->replyNext == NULL);

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
        reply_push(sc_donated, reply);

        /* now do the actual donation */
        schedContext_donate(sc_donated, tcb_callee);
    }
}

void break_reply_chain_at(reply_t *reply)
{
    reply_t *prev = reply->replyPrev;
    reply_t *next = reply->replyNext;

    if (prev) {
        prev->replyNext = NULL;
    }
    if (next) {
        next->replyPrev = NULL;
    }
    reply->replyPrev = NULL;
    reply->replyNext = NULL;

    sched_context_t *replySC = reply->replySC;

    if (replySC) {
        replySC->scReply = NULL;
        reply->replySC = NULL;
    }
}

/* Remove a reply from the call stack */
void reply_remove(reply_t *reply, tcb_t *tcb)
{
    assert(reply->replyTCB == tcb);
    assert(thread_state_get_tsType(tcb->tcbState) == ThreadState_BlockedOnReply);
    assert(thread_state_get_replyObject(tcb->tcbState) == REPLY_REF(reply));

    sched_context_t *replySC = reply->replySC;

    if (likely(replySC)) {
        reply_pop(reply);

        /* give it back */
        if (tcb->tcbSchedContext == NULL) {
            /* only give the SC back if our SC is NULL. This prevents
             * strange behaviour when a thread is bound to an sc while it is
             * in the BlockedOnReply state. The semantics in this case are that the
             * SC cannot go back to the caller if the caller has received another one */
            schedContext_donate(replySC, tcb);
        }
    } else {
        break_reply_chain_at(reply);
    }

    reply_unlink(reply, tcb);
}

void reply_remove_tcb(tcb_t *tcb)
{
    assert(thread_state_get_tsType(tcb->tcbState) == ThreadState_BlockedOnReply);
    reply_t *reply = REPLY_PTR(thread_state_get_replyObject(tcb->tcbState));

    break_reply_chain_at(reply);
    reply_unlink(reply, tcb);
}
