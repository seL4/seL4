/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_UNTYPED_H
#define __OBJECT_UNTYPED_H

#include <types.h>
#include <api/failures.h>
#include <api/types.h>
#include <object/structures.h>
#include <object/cnode.h>

#define MIN_SIZE_BITS 4
#define MAX_FREE_INDEX(sizeBits) (BIT((sizeBits) - MIN_SIZE_BITS))
#define FREE_INDEX_TO_OFFSET(freeIndex) ((freeIndex)<<MIN_SIZE_BITS)
#define GET_FREE_REF(base,freeIndex) ((word_t)(((word_t)(base)) + FREE_INDEX_TO_OFFSET(freeIndex)))
#define GET_FREE_INDEX(base,free) (((word_t)(free) - (word_t)(base))>>MIN_SIZE_BITS)

exception_t decodeUntypedInvocation(word_t label, unsigned int length,
                                    cte_t *slot, cap_t cap,
                                    extra_caps_t extraCaps, bool_t call,
                                    word_t *buffer);
exception_t invokeUntyped_Retype(cte_t *srcSlot, void* base_ign,
                                 void* freeRegionBase, object_t newType,
                                 word_t userSize, slot_range_t destSlots,
                                 bool_t call);

#endif
