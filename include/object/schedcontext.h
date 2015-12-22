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
exception_t invokeSchedContext_UnbindTCB(sched_context_t *sc);
exception_t invokeSchedContext_BindTCB(sched_context_t *sc, tcb_t *tcb);
exception_t decodeSchedContext_BindTCB(sched_context_t *sc, extra_caps_t rootCaps);

void bindSchedContext(sched_context_t *sc, tcb_t *tcb);
void unbindSchedContext(sched_context_t *sc);

#endif
