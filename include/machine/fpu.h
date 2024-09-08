/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <object/structures.h>
#include <model/statedata.h>
#include <arch/machine/fpu.h>

#ifdef CONFIG_HAVE_FPU

/* Perform any actions required for the deletion of the given thread. */
void fpuThreadDelete(tcb_t *thread);

void switchLocalFpuOwner(user_fpu_state_t *new_owner);

/* Switch the current owner of the FPU state on the core specified by 'cpu'. */
void switchFpuOwner(user_fpu_state_t *new_owner, word_t cpu);

/* Returns whether or not the passed thread is using the current active fpu state */
static inline bool_t nativeThreadUsingFPU(tcb_t *thread)
{
    return &thread->tcbArch.tcbContext.fpuState ==
           NODE_STATE_ON_CORE(ksActiveFPUState, thread->tcbAffinity);
}

/* Called without global lock held! */
static inline void FORCE_INLINE lazyFPURestore(tcb_t *thread)
{
    if (thread->flags & seL4_TCBFlag_fpuDisabled) {
        disableFpu();
    } else if (nativeThreadUsingFPU(thread)) {
        enableFpu();
    } else {
        switchLocalFpuOwner(&thread->tcbArch.tcbContext.fpuState);
    }
}

#endif /* CONFIG_HAVE_FPU */

