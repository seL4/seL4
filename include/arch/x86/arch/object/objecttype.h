/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_OBJECTTYPE_H
#define __ARCH_OBJECT_OBJECTTYPE_H

#include <types.h>
#include <api/failures.h>
#include <api/types.h>
#include <object/structures.h>
#include <object/cnode.h>

deriveCap_ret_t Arch_deriveCap(cte_t *slot, cap_t cap);
cap_t CONST Arch_updateCapData(bool_t preserve, word_t data, cap_t cap);
cap_t CONST Arch_maskCapRights(seL4_CapRights_t cap_rights_mask, cap_t cap);
finaliseCap_ret_t Arch_finaliseCap(cap_t cap, bool_t final);
bool_t CONST Arch_hasRecycleRights(cap_t cap);
bool_t CONST Arch_sameRegionAs(cap_t cap_a, cap_t cap_b);
bool_t CONST Arch_sameObjectAs(cap_t cap_a, cap_t cap_b);
bool_t CONST Arch_isFrameType(word_t type);
cap_t Arch_createObject(object_t t, void *regionBase, word_t userSize, bool_t deviceMemory);
exception_t Arch_decodeInvocation(word_t invLabel, word_t length, cptr_t cptr, cte_t *slot, cap_t cap, extra_caps_t excaps, bool_t call, word_t* buffer);
void Arch_prepareThreadDelete(tcb_t *thread);
word_t Arch_getObjectSize(word_t t);

/* the following functions are specific to 32-bit and 64-bit */

deriveCap_ret_t Mode_deriveCap(cte_t *slot, cap_t cap);
finaliseCap_ret_t Mode_finaliseCap(cap_t cap, bool_t final);
bool_t CONST Mode_sameRegionAs(cap_t cap_a, cap_t cap_b);
cap_t Mode_createObject(object_t t, void *regionBase, word_t userSize, bool_t deviceMemory);
exception_t Mode_decodeInvocation(word_t invLabel, word_t length, cptr_t cptr, cte_t *slot, cap_t cap, extra_caps_t excaps, word_t* buffer);
word_t Mode_getObjectSize(word_t t);

void Arch_postCapDeletion(cap_t cap);

#endif
