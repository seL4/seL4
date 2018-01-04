/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __MODE_KERNEL_TLB_H
#define __MODE_KERNEL_TLB_H

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
    invalidateLocalPCID(INVPCID_TYPE_SINGLE, (void*)0, asid);
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

#endif /* __MODE_KERNEL_TLB_H */
