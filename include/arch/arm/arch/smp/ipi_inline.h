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

#ifndef __ARCH_SMP_IPI_INLINE_H
#define __ARCH_SMP_IPI_INLINE_H

#include <config.h>
#include <smp/ipi.h>

#ifdef ENABLE_SMP_SUPPORT
static inline void doRemoteStall(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_Stall, cpu);
}

#ifdef CONFIG_HAVE_FPU
static inline void doRemoteswitchFpuOwner(user_fpu_state_t *new_owner, word_t cpu)
{
    doRemoteOp1Arg(IpiRemoteCall_switchFpuOwner, (word_t)new_owner, cpu);
}
#endif /* CONFIG_HAVE_FPU */

static inline void doRemoteInvalidateTranslationSingle(vptr_t vptr, word_t mask)
{
    doRemoteMaskOp1Arg(IpiRemoteCall_InvalidateTranslationSingle, vptr, mask);
}

static inline void doRemoteInvalidateTranslationASID(asid_t asid, word_t mask)
{
    doRemoteMaskOp1Arg(IpiRemoteCall_InvalidateTranslationASID, asid, mask);
}

static inline void doRemoteInvalidateTranslationAll(word_t mask)
{
    doRemoteMaskOp0Arg(IpiRemoteCall_InvalidateTranslationAll, mask);
}
#endif /* ENABLE_SMP_SUPPORT */
#endif /* __ARCH_SMP_IPI_INLINE_H */
