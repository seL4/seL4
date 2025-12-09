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
cap_t Arch_createObject(object_t t, void *regionBase, word_t userSize, bool_t deviceMemory);
exception_t Arch_decodeInvocation(word_t label, word_t length,
                                  cptr_t cptr, cte_t *slot, cap_t cap,
                                  bool_t call, word_t *buffer);
void Arch_prepareThreadDelete(tcb_t *thread);
word_t Arch_getObjectSize(word_t t);
bool_t Arch_isFrameType(word_t type);

static inline void Arch_postCapDeletion(cap_t cap)
{
}

/**
 * Return true if the given arch cap can be a descendant of an IRQControlCap.
 */
static inline CONST bool_t Arch_isIRQControlDescendant(cap_t cap)
{
    return false;
}

/**
 * isMDBParentOf for architecture caps. See Arm version for more detailed docs.
 */
static inline CONST bool_t Arch_isMDBParentOf(cap_t cap_a, cap_t cap_b, bool_t firstBadged)
{
    switch (cap_get_capType(cap_a)) {
#ifdef CONFIG_ALLOW_SBI_CALLS
    case cap_sbi_cap: {
        if (!cap_sbi_cap_get_capSBIEIDBadged(cap_a)) {
            /* If there is no EID badge then the cap cannot be badged. */
            return true;
        }

        if (!cap_sbi_cap_get_capSBIEIDBadged(cap_b)) {
            return false;
        }

        word_t eid_a_badge = cap_sbi_cap_get_capSBIEIDBadge(cap_a);
        word_t eid_b_badge = cap_sbi_cap_get_capSBIEIDBadge(cap_b);

        if (eid_a_badge != eid_b_badge) {
            return false;
        }

        if (!cap_sbi_cap_get_capSBIFIDBadged(cap_a)) {
            return !firstBadged;
        }

        if (!cap_sbi_cap_get_capSBIFIDBadged(cap_b)) {
            return false;
        }

        word_t fid_a_badge = cap_sbi_cap_get_capSBIFIDBadge(cap_a);
        word_t fid_b_badge = cap_sbi_cap_get_capSBIFIDBadge(cap_b);
        return (fid_a_badge == fid_b_badge && !firstBadged);
        break;
    }
#endif

    default:
        return true;
        break;
    }
}
