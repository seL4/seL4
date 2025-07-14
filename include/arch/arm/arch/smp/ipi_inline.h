/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <smp/ipi.h>

#ifdef ENABLE_SMP_SUPPORT
static inline void doRemoteStall(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_Stall, cpu);
}

#ifdef CONFIG_HAVE_FPU
static inline void doRemoteswitchFpuOwner(tcb_t *new_owner, word_t cpu)
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

static inline void doRemoteMaskPrivateInterrupt(word_t cpu, word_t disable, word_t irq)
{
    doRemoteOp2Arg(IpiRemoteCall_MaskPrivateInterrupt, disable, irq, cpu);
}

#ifdef CONFIG_ARM_GIC_V3_SUPPORT
static inline void doRemoteDeactivatePrivateInterrupt(word_t cpu, word_t irq)
{
    doRemoteOp1Arg(IpiRemoteCall_DeactivatePrivateInterrupt, irq, cpu);
}
#endif /* CONFIG_ARM_GIC_V3_SUPPORT */
#endif /* ENABLE_SMP_SUPPORT */

