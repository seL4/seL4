/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <smp/ipi.h>
#include <arch/kernel/tlb.h>
#include <mode/smp/ipi.h>
#include <arch/kernel/tlb_bitmap.h>

/*
 * This is a wrapper around invalidatePCID that can be used to invalidate
 * an ASID and, in the case of SMP, potentially clear a vspace of having
 * any translations on this core
 */
static inline void invalidateLocalASID(vspace_root_t *vspace, asid_t asid)
{
    invalidateLocalPCID(INVPCID_TYPE_SINGLE, (void *)0, asid);
#ifdef ENABLE_SMP_SUPPORT
    if (pptr_to_paddr(vspace) != getCurrentUserVSpaceRoot()) {
        tlb_bitmap_unset(vspace, getCurrentCPUIndex());
    }
#endif /* ENABLE_SMP_SUPPORT */
}

static inline void invalidatePCID(word_t type, void *vaddr, asid_t asid, word_t mask)
{
    invalidateLocalPCID(type, vaddr, asid);
    SMP_COND_STATEMENT(doRemoteInvalidatePCID(type, vaddr, asid, mask));
}

static inline void invalidateASID(vspace_root_t *vspace, asid_t asid, word_t mask)
{
    invalidateLocalASID(vspace, asid);
    SMP_COND_STATEMENT(doRemoteInvalidateASID(vspace, asid, mask));
}

