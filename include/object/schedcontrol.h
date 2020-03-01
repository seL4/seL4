/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __OBJECT_SCHED_CONTROL_H
#define __OBJECT_SCHED_CONTROL_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

exception_t decodeSchedControlInvocation(word_t label, cap_t cap, word_t length, extra_caps_t extra_caps,
                                         word_t *buffer);

#endif /* __OBJECT_SCHED_CONTROL_H */
