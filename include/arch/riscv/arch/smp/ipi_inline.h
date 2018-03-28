/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 */

#ifndef __ARCH_SMP_IPI_INLINE_H
#define __ARCH_SMP_IPI_INLINE_H

#include <config.h>
#include <smp/ipi.h>
#include <arch/sbi.h>

#ifdef ENABLE_SMP_SUPPORT
// RVTODO: these functions appear unused
static inline void doRemoteStall(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_Stall, cpu);
}

static inline void doRemoteInvalidateTranslationSingle(vptr_t vptr, word_t mask)
{
    sbi_remote_sfence_vma(&mask, vptr, sizeof(vptr_t));
}

static inline void doRemoteInvalidateTranslationASID(asid_t asid, word_t mask)
{
    sbi_remote_sfence_vma_asid(&mask, 0, 0, asid);
}

static inline void doRemoteInvalidateTranslationAll(word_t mask)
{
    sbi_remote_fence_i(&mask);
}
#endif /* ENABLE_SMP_SUPPORT */
#endif /* __ARCH_SMP_IPI_INLINE_H */
