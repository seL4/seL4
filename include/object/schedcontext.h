/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_SCHED_CONTEXT_H
#define __OBJECT_SCHED_CONTEXT_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

exception_t decodeSchedContextInvocation(word_t label, cap_t cap, extra_caps_t extraCaps);
exception_t invokeSchedContext_Yield(sched_context_t *sched_context);
exception_t invokeSchedContext_Bind(sched_context_t *sc, cap_t cap);
exception_t decodeSchedContext_Bind(sched_context_t *sc, extra_caps_t rootCaps);
exception_t invokeSchedContext_Unbind(sched_context_t *sc);
exception_t decodeSchedContext_Unbind(sched_context_t *sc);
exception_t decodeSchedContext_UnbindObject(sched_context_t *sc, extra_caps_t rootCaps);
exception_t invokeSchedContext_UnbindObject(sched_context_t *sc, cap_t cap);
exception_t decodeSchedContext_YieldTo(sched_context_t *sc);
exception_t invokeSchedContext_YieldTo(sched_context_t *sc);
exception_t invokeSchedContext_Consumed(sched_context_t *sc);

/* bind a tcb to a scheduling context, this will set the scheduling context's
 * home to the tcb */
void schedContext_bindTCB(sched_context_t *sc, tcb_t *tcb);
/* unbind any and all tcbs associated with this scheduling context */
void schedContext_unbindAllTCBs(sched_context_t *sc);
/* remove a specific TCB's reference from a scheduling context */
void schedContext_removeTCB(sched_context_t *sc, tcb_t *tcb);

void schedContext_bindNtfn(sched_context_t *sc, notification_t *ntfn);
void schedContext_unbindNtfn(sched_context_t *sc);
void schedContext_donate(tcb_t *to, sched_context_t *sc);
void schedContext_resume(sched_context_t *sc);
time_t schedContext_updateConsumed(sched_context_t *sc);
void schedContext_completeYieldTo(tcb_t *yielder);

#endif
