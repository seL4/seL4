/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __ARCH_SMP_IPI_INLINE_H
#define __ARCH_SMP_IPI_INLINE_H

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

#endif

#endif /* __ARCH_SMP_IPI_INLINE_H */
