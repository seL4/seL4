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

#define GET_FREE_REF(base,freeIndex) ((word_t)(((word_t)(base)) + (freeIndex)))

exception_t decodeUntypedInvocation(word_t label, word_t length,
                                    cte_t *slot, cap_t cap,
                                    extra_caps_t extraCaps, bool_t call,
                                    word_t *buffer);
exception_t invokeUntyped_Retype(cte_t *srcSlot, void* base_ign,
                                 void* freeRegionBase, object_t newType,
                                 unsigned int objSize, slot_range_t destSlots,
                                 bool_t call, bool_t deviceMemory);

#endif
