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

#ifndef __ARCH_KERNEL_TLB_H
#define __ARCH_KERNEL_TLB_H

#include <arch/smp/ipi_inline.h>

static inline void invalidatePageStructureCacheASID(paddr_t root, asid_t asid, word_t mask)
{
    invalidateLocalPageStructureCacheASID(root, asid);
    SMP_COND_STATEMENT(doRemoteInvalidatePageStructureCacheASID(root, asid, mask));
}

static inline void invalidateTranslationSingle(vptr_t vptr, word_t mask)
{
    invalidateLocalTranslationSingle(vptr);
    SMP_COND_STATEMENT(doRemoteInvalidateTranslationSingle(vptr, mask));
}

static inline void invalidateTranslationSingleASID(vptr_t vptr, asid_t asid, word_t mask)
{
    invalidateLocalTranslationSingleASID(vptr, asid);
    SMP_COND_STATEMENT(doRemoteInvalidateTranslationSingleASID(vptr, asid, mask));
}

static inline void invalidateTranslationAll(word_t mask)
{
    invalidateLocalTranslationAll();
    SMP_COND_STATEMENT(doRemoteInvalidateTranslationAll(mask));
}

#endif /* __ARCH_KERNEL_TLB_H */
