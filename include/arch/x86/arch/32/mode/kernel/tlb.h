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

#include <mode/smp/ipi.h>
#include <arch/kernel/tlb.h>

static inline void invalidateTLBEntry(vptr_t vptr, word_t mask)
{
    invalidateLocalTLBEntry(vptr);
    SMP_COND_STATEMENT(doRemoteInvalidateTLBEntry(vptr, mask));
}

static inline void invalidatePageStructureCache(word_t mask)
{
    invalidateLocalPageStructureCache();
    SMP_COND_STATEMENT(doRemoteInvalidatePageStructureCache(mask));
}

static inline void invalidateTLB(word_t mask)
{
    invalidateLocalTLB();
    SMP_COND_STATEMENT(doRemoteInvalidateTLB(mask));
}

#endif /* __MODE_KERNEL_TLB_H */
