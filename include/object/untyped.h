/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <api/types.h>
#include <object/structures.h>
#include <object/cnode.h>

exception_t decodeUntypedInvocation(word_t invLabel, word_t length,
                                    cte_t *slot, cap_t cap,
                                    bool_t call, word_t *buffer);
exception_t invokeUntyped_Retype(cte_t *srcSlot,
                                 void *retypeBase, object_t newType, word_t userSize, word_t total_size,
                                 cte_t *destCNode, word_t destOffset, word_t destLength,
                                 bool_t deviceMemory);
