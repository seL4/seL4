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

static inline int vtd_get_root_index(dev_id_t dev)
{
    return (dev >> 8) & 0xff;
}

static inline int vtd_get_context_index(dev_id_t dev)
{
    return dev & 0xff;
}

struct lookupIOPTSlot_ret {
    exception_t status;
    vtd_pte_t*  iopt;
    int         index;
    int         level;
};
typedef struct lookupIOPTSlot_ret lookupIOPTSlot_ret_t;

cap_t master_iospace_cap(void);
exception_t decodeIA32IOPTInvocation(word_t label, uint32_t length, cte_t* slot, cap_t cap, extra_caps_t extraCaps, word_t*  buffer);
exception_t decodeIA32IOMapInvocation(word_t label, uint32_t length, cte_t* slot, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
void unmapIOPage(cap_t cap);
vtd_cte_t *lookupVTDContextSlot(cap_t cap);
void unmapVTDContextEntry(cap_t cap);
void unmapVTDPT(vtd_pte_t *parent, vtd_pte_t *child, uint32_t index);
void unmapAllIOPT(vtd_pte_t *pt, int level);
void unmapIOPTCap(cap_t cap);

#endif
