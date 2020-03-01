/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
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
