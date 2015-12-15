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

exception_t decodeSchedControlInvocation(word_t label, word_t length, extra_caps_t extra_caps,
                                         word_t *buffer);
exception_t decodeSchedControl_Configure(word_t length, extra_caps_t extra_caps, word_t *buffer);
exception_t invokeSchedControl_Configure(sched_context_t *sched_context, time_t budget, time_t period);

#endif
