/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __OBJECT_SCHED_CONTROL_H
#define __OBJECT_SCHED_CONTROL_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

exception_t decodeSchedControlInvocation(word_t label, cap_t cap, word_t length, extra_caps_t extra_caps,
                                         word_t *buffer);

#endif /* __OBJECT_SCHED_CONTROL_H */
