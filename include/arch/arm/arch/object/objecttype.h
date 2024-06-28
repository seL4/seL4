/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <api/types.h>
#include <object/structures.h>
#include <object/cnode.h>

deriveCap_ret_t Arch_deriveCap(cte_t *slot, cap_t cap);
cap_t CONST Arch_updateCapData(bool_t preserve, word_t data, cap_t cap);
cap_t CONST Arch_maskCapRights(seL4_CapRights_t cap_rights_mask, cap_t cap);
finaliseCap_ret_t Arch_finaliseCap(cap_t cap, bool_t final);
bool_t CONST Arch_sameRegionAs(cap_t cap_a, cap_t cap_b);
bool_t CONST Arch_sameObjectAs(cap_t cap_a, cap_t cap_b);
bool_t CONST Arch_isFrameType(word_t type);
cap_t Arch_createObject(object_t t, void *regionBase, word_t userSize, bool_t deviceMemory);
exception_t Arch_decodeInvocation(word_t invLabel, word_t length,
                                  cptr_t cptr, cte_t *slot, cap_t cap,
                                  bool_t call, word_t *buffer);
void Arch_prepareThreadDelete(tcb_t *thread);
word_t Arch_getObjectSize(word_t t);

static inline void Arch_postCapDeletion(cap_t cap)
{
}

/**
 * Return true if the given arch cap can be a descendant of an IRQControlCap.
 */
static inline CONST bool_t Arch_isIRQControlDescendant(cap_t cap)
{
#if CONFIG_MAX_NUM_NODES == 1
    return cap_get_capType(cap) == cap_sgi_signal_cap;
#else
    return false;
#endif
}

/**
 * isMDBParentOf for architecture caps. For most arch caps, this just returns
 * true, but if there are badged versions of arch caps, this functions should
 * perform the necessary checks.
 *
 * Called by the generic isMDBParentOf after the revocable bit has been checked
 * and sameRegionAs has been established. This means we can assume both as true
 * inside Arch_isMDBParentOf.
 */
static inline CONST bool_t Arch_isMDBParentOf(cap_t cap_a, cap_t cap_b, bool_t firstBadged)
{
    switch (cap_get_capType(cap_a)) {
#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        return !firstBadged;
        break;
#endif

#ifdef CONFIG_ALLOW_SMC_CALLS
    case cap_smc_cap: {
        word_t badge;

        badge = cap_smc_cap_get_capSMCBadge(cap_a);
        if (badge == 0) {
            return true;
        }
        return (badge == cap_smc_cap_get_capSMCBadge(cap_b)) &&
               !firstBadged;
        break;
    }
#endif

    default:
        return true;
    }
}
