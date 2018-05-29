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
#ifndef __OBJECT_REPLY_H
#define __OBJECT_REPLY_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

/* Unlink a reply from its tcb */
static inline void reply_unlink(reply_t *reply)
{
    /* check the tcb and reply are linked correctly */
    assert(thread_state_get_replyObject(reply->replyTCB->tcbState) == REPLY_REF(reply));

    tcb_t *tcb = reply->replyTCB;
    thread_state_ptr_set_replyObject(&tcb->tcbState, REPLY_REF(0));
    reply->replyTCB = NULL;
    setThreadState(tcb, ThreadState_Inactive);
}

/* Push a reply object onto the call stack */
void reply_push(tcb_t *tcb_caller, tcb_t *tcb_callee, reply_t *reply, bool_t canDonate);
/* Pop the head reply from the call stack */
void reply_pop(reply_t *reply);
/* Remove a reply from the call stack - replyTCB must be in ThreadState_BlockedOnReply */
void reply_remove(reply_t *reply);
/* Remove a specific tcb, and the reply it is blocking on, from the call stack */
void reply_remove_tcb(tcb_t *tcb);

#endif /* __OBJECT_REPLY_H */
