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

#ifndef __ARCH_KERNEL_IPI_INLINE_H
#define __ARCH_KERNEL_IPI_INLINE_H

#include <config.h>
#include <smp/ipi.h>

#ifdef ENABLE_SMP_SUPPORT
static inline void doRemoteStall(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_Stall, cpu);
}

static inline void doRemoteswitchFpuOwner(user_fpu_state_t *new_owner, word_t cpu)
{
    doRemoteOp1Arg(IpiRemoteCall_switchFpuOwner, (word_t)new_owner, cpu);
}

static inline void doRemoteInvalidatePageStructureCacheASID(paddr_t root, asid_t asid, word_t mask)
{
    doRemoteMaskOp2Arg(IpiRemoteCall_InvalidatePageStructureCacheASID, root, asid, mask);
}

static inline void doRemoteInvalidateTranslationSingle(vptr_t vptr, word_t mask)
{
    doRemoteMaskOp1Arg(IpiRemoteCall_InvalidateTranslationSingle, vptr, mask);
}

static inline void doRemoteInvalidateTranslationSingleASID(vptr_t vptr, asid_t asid, word_t mask)
{
    doRemoteMaskOp2Arg(IpiRemoteCall_InvalidateTranslationSingleASID, vptr, asid, mask);
}

static inline void doRemoteInvalidateTranslationAll(word_t mask)
{
    doRemoteMaskOp0Arg(IpiRemoteCall_InvalidateTranslationAll, mask);
}

#ifdef CONFIG_VTX
static inline void
doRemoteClearCurrentVCPU(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_ClearCurrentVCPU, cpu);
}

static inline void
doRemoteVMCheckBoundNotification(word_t cpu, tcb_t *tcb)
{
    doRemoteOp1Arg(IpiRemoteCall_VMCheckBoundNotification, cpu, (word_t)tcb);
}
#endif

#endif /* ENABLE_SMP_SUPPORT */
#endif /* __ARCH_KERNEL_IPI_INLINE_H */
