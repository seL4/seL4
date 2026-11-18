/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

/* Unlink a reply from its tcb */
static inline void reply_unlink(reply_t *reply, tcb_t *tcb)
{
    /* check that the tcb has a thread state with reply */
    assert(thread_state_get_tsType(tcb->tcbState) == ThreadState_BlockedOnReceive ||
           thread_state_get_tsType(tcb->tcbState) == ThreadState_BlockedOnReply);

    /* check the tcb and reply are linked correctly */
    assert(reply->replyTCB == tcb);
    assert(thread_state_get_replyObject(tcb->tcbState) == REPLY_REF(reply));

    reply->replyTCB = NULL;
    /* This means the value of the thread state reply reference no longer matters. */
    setThreadState(tcb, ThreadState_Inactive);
}

static inline void reply_push(sched_context_t *sc, reply_t *reply)
{
    reply_t *last_caller = sc->scReply;

    /* check stack integrity */
    assert(last_caller == NULL || last_caller->replySC == sc);

    /* Retain the order of the following updates for verification */

    /* if there was an old caller, first unlink it from the sc */
    if (unlikely(last_caller)) {
        last_caller->replySC = NULL;
    }
    /* push reply onto the stack */
    reply->replyNext = NULL;
    reply->replyPrev = last_caller;
    if (unlikely(last_caller)) {
        last_caller->replyNext = reply;
    }
    /* link the new head of the stack to the sc */
    sc->scReply = reply;
    reply->replySC = sc;
}

static inline void reply_pop(reply_t *reply)
{
    sched_context_t *replySC = reply->replySC;

    reply_t *prev = reply->replyPrev;

    assert(replySC != NULL);

    /* Retain the order of the following updates for verification */

    /* first unlink the head of the stack from the sc */
    reply->replySC = NULL;
    /* pop reply from the stack */
    reply->replyPrev = NULL;
    if (unlikely(prev)) {
        prev->replyNext = NULL;
    }
    /* link the new head (if any) of the stack to the sc */
    replySC->scReply = prev;
    if (unlikely(prev)) {
        prev->replySC = replySC;
    }
}

/* Push a reply object onto the call stack */
void reply_link_caller(tcb_t *tcb_caller, tcb_t *tcb_callee, reply_t *reply, bool_t canDonate);
void break_reply_chain_at(reply_t *reply);
/* Remove a reply from the call stack - replyTCB must be in ThreadState_BlockedOnReply */
void reply_remove(reply_t *reply, tcb_t *tcb);
/* Remove a specific tcb, and the reply it is blocking on, from the call stack */
void reply_remove_tcb(tcb_t *tcb);

