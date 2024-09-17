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

/* It is assumed that every untyped is within seL4_MinUntypedBits and seL4_MaxUntypedBits
 * (inclusive). This means that every untyped stored as seL4_MinUntypedBits
 * subtracted from its size before it is stored in capBlockSize, and
 * capFreeIndex counts in chunks of size 2^seL4_MinUntypedBits. The seL4_MaxUntypedBits
 * is the minimal untyped that can be stored when considering both how
 * many bits of capBlockSize there are, and the largest offset that can
 * be stored in capFreeIndex */
#define MAX_FREE_INDEX(sizeBits) (BIT((sizeBits) - seL4_MinUntypedBits))
#define FREE_INDEX_TO_OFFSET(freeIndex) ((freeIndex)<<seL4_MinUntypedBits)
#define GET_FREE_REF(base,freeIndex) ((pptr_t)(((pptr_t)(base)) + FREE_INDEX_TO_OFFSET(freeIndex)))
#define GET_FREE_INDEX(base,free) (((word_t)(free) - (word_t)(base))>>seL4_MinUntypedBits)
#define GET_OFFSET_FREE_PTR(base, offset) ((void *)(((pptr_t)(base)) + (offset)))
#define OFFSET_TO_FREE_INDEX(offset) ((offset)>>seL4_MinUntypedBits)

exception_t decodeUntypedInvocation(word_t invLabel, word_t length,
                                    cte_t *slot, cap_t cap,
                                    bool_t call, register_t *buffer);
exception_t invokeUntyped_Retype(cte_t *srcSlot, bool_t reset,
                                 void *retypeBase, object_t newType, word_t userSize,
                                 cte_t *destCNode, word_t destOffset, word_t destLength,
                                 bool_t deviceMemory);
