/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_IOSPACE_H
#define __ARCH_OBJECT_IOSPACE_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

struct lookupIOPTSlot_ret {
    exception_t status;
    vtd_pte_t*  ioptSlot;
    int         level;
};
typedef struct lookupIOPTSlot_ret lookupIOPTSlot_ret_t;

cap_t master_iospace_cap(void);
exception_t decodeIA32IOPTInvocation(word_t label, uint32_t length, cte_t* slot, cap_t cap, extra_caps_t extraCaps, word_t*  buffer);
exception_t decodeIA32IOMapInvocation(word_t label, uint32_t length, cte_t* slot, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
exception_t decodeIA32IOUnMapInvocation(word_t label, uint32_t length, cte_t* slot, cap_t cap, extra_caps_t extraCaps);
exception_t decodeIA32IOSpaceInvocation(word_t label, cap_t cap);
void unmapIOPage(cap_t cap);
void deleteIOPageTable(cap_t cap);

#endif
