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
    /* check the tcb and reply are linked correctly */
    assert(reply->replyTCB == tcb);
    assert(thread_state_get_replyObject(tcb->tcbState) == REPLY_REF(reply));

    thread_state_ptr_set_replyObject(&tcb->tcbState, REPLY_REF(0));
    reply->replyTCB = NULL;
    setThreadState(tcb, ThreadState_Inactive);
}

/* Push a reply object onto the call stack */
void reply_push(tcb_t *tcb_caller, tcb_t *tcb_callee, reply_t *reply, bool_t canDonate);
/* Pop the head reply from the call stack */
void reply_pop(reply_t *reply, tcb_t *tcb);
/* Remove a reply from the call stack - replyTCB must be in ThreadState_BlockedOnReply */
void reply_remove(reply_t *reply, tcb_t *tcb);
/* Remove a specific tcb, and the reply it is blocking on, from the call stack */
void reply_remove_tcb(tcb_t *tcb);

