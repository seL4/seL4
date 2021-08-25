/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/fpu.h>
#include <api/failures.h>
#include <model/statedata.h>
#include <arch/object/structures.h>

#ifdef CONFIG_HAVE_FPU
/* Switch the owner of the FPU to the given thread on local core. */
void switchLocalFpuOwner(user_fpu_state_t *new_owner)
{
    enableFpu();
    if (NODE_STATE(ksActiveFPUState)) {
        saveFpuState(NODE_STATE(ksActiveFPUState));
    }
    if (new_owner) {
        NODE_STATE(ksFPURestoresSinceSwitch) = 0;
        loadFpuState(new_owner);
    } else {
        disableFpu();
    }
    NODE_STATE(ksActiveFPUState) = new_owner;
}

void switchFpuOwner(user_fpu_state_t *new_owner, word_t cpu)
{
#ifdef ENABLE_SMP_SUPPORT
    if (cpu != getCurrentCPUIndex()) {
        doRemoteswitchFpuOwner(new_owner, cpu);
    } else
#endif /* ENABLE_SMP_SUPPORT */
    {
        switchLocalFpuOwner(new_owner);
    }
}

/* Handle an FPU fault.
 *
 * This CPU exception is thrown when userspace attempts to use the FPU while
 * it is disabled. We need to save the current state of the FPU, and hand
 * it over. */
exception_t handleFPUFault(void)
{
    /* If we have already given the FPU to the user, we should not reach here.
     * This should only be able to occur on CPUs without an FPU at all, which
     * we presumably are happy to assume will not be running seL4. */
    assert(!nativeThreadUsingFPU(NODE_STATE(ksCurThread)));

    /* Otherwise, lazily switch over the FPU. */
    switchLocalFpuOwner(&NODE_STATE(ksCurThread)->tcbArch.tcbContext.fpuState);

    return EXCEPTION_NONE;
}

/* Prepare for the deletion of the given thread. */
void fpuThreadDelete(tcb_t *thread)
{
    /* If the thread being deleted currently owns the FPU, switch away from it
     * so that 'ksActiveFPUState' doesn't point to invalid memory. */
    if (nativeThreadUsingFPU(thread)) {
        switchFpuOwner(NULL, SMP_TERNARY(thread->tcbAffinity, 0));
    }
}
#endif /* CONFIG_HAVE_FPU */
