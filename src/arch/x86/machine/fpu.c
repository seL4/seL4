/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <api/failures.h>
#include <api/syscall.h>
#include <model/statedata.h>
#include <arch/machine/fpu.h>
#include <arch/machine/cpu_registers.h>
#include <arch/object/structures.h>
#include <arch/linker.h>

/*
 * Setup the FPU register state for a new thread.
 */
void
Arch_initFpuContext(user_context_t *context)
{
    if (config_set(CONFIG_FXSAVE)) {
        context->fpuState = x86KSnullFpuState;
    } else {
        /* the 0 state is good enough for XSAVE */
    }
}

/*
 * Switch the owner of the FPU to the given thread on local core.
 */
void
switchLocalFpuOwner(user_fpu_state_t *new_owner)
{
    enableFpu();
    if (ARCH_NODE_STATE(x86KSActiveFPUState)) {
        saveFpuState(ARCH_NODE_STATE(x86KSActiveFPUState));
    }
    if (new_owner) {
        loadFpuState(new_owner);
    } else {
        disableFpu();
    }
    ARCH_NODE_STATE(x86KSActiveFPUState) = new_owner;
}

void
switchFpuOwner(user_fpu_state_t *new_owner, word_t cpu)
{
#if CONFIG_MAX_NUM_NODES > 1
    if (cpu != getCurrentCPUIndex()) {
        doRemoteswitchFpuOwner(new_owner, cpu);
    } else
#endif /* CONFIG_MAX_NUM_NODES */
    {
        switchLocalFpuOwner(new_owner);
    }
}

/*
 * Handle a FPU fault.
 *
 * This CPU exception is thrown when userspace attempts to use the FPU while
 * it is disabled. We need to save the current state of the FPU, and hand
 * it over.
 */
VISIBLE exception_t
handleUnimplementedDevice(void)
{
    /*
     * If we have already given the FPU to the user, we should not reach here.
     *
     * This should only be able to occur on CPUs without an FPU at all, which
     * we presumably are happy to assume will not be running seL4.
     */
    assert(!nativeThreadUsingFPU(NODE_STATE(ksCurThread)));

    /* Otherwise, lazily switch over the FPU. */
    switchLocalFpuOwner(&NODE_STATE(ksCurThread)->tcbArch.tcbContext.fpuState);

    return EXCEPTION_NONE;
}

/*
 * Prepare for the deletion of the given thread.
 */
void
Arch_fpuThreadDelete(tcb_t *thread)
{
    /*
     * If the thread being deleted currently owns the FPU, switch away from it
     * so that 'x86KSActiveFPUState' doesn't point to invalid memory.
     */
    if (nativeThreadUsingFPU(thread)) {
        switchFpuOwner(NULL, SMP_TERNARY(thread->tcbAffinity, 0));
    }
}

/*
 * Initialise the FPU for this machine.
 */
BOOT_CODE bool_t
Arch_initFpu(void)
{
    /* Enable FPU / SSE / SSE2 / SSE3 / SSSE3 / SSE4 Extensions. */
    write_cr4(read_cr4() | CR4_OSFXSR);

    /* Enable the FPU in general. */
    write_cr0((read_cr0() & ~CR0_EMULATION) | CR0_MONITOR_COPROC | CR0_NUMERIC_ERROR);
    enableFpu();

    /* Initialize the fpu state */
    finit();

    if (config_set(CONFIG_XSAVE)) {
        uint64_t xsave_features;
        uint32_t xsave_instruction;
        uint64_t desired_features = config_default(CONFIG_XSAVE_FEATURE_SET, 1);
        /* check for XSAVE support */
        if (!(x86_cpuid_ecx(1, 0) & BIT(26))) {
            printf("XSAVE not supported\n");
            return false;
        }
        /* enable XSAVE support */
        write_cr4(read_cr4() | CR4_OSXSAVE);
        /* check feature mask */
        xsave_features = ((uint64_t)x86_cpuid_edx(0x0d, 0x0) << 32) | x86_cpuid_eax(0x0d, 0x0);
        if ((xsave_features & desired_features) != desired_features) {
            printf("Requested feature mask is 0x%llx, but only 0x%llx supported\n", desired_features, (long long)xsave_features);
            return false;
        }
        /* enable feature mask */
        write_xcr0(desired_features);
        /* validate the xsave buffer size and instruction */
        if (x86_cpuid_ebx(0x0d, 0x0) != CONFIG_XSAVE_SIZE) {
            printf("XSAVE buffer set set to %d, but should be %d\n", CONFIG_XSAVE_SIZE, x86_cpuid_ecx(0x0d, 0x0));
            return false;
        }
        /* check if a specialized XSAVE instruction was requested */
        xsave_instruction = x86_cpuid_eax(0x0d, 0x1);
        if (config_set(CONFIG_XSAVE_XSAVEOPT)) {
            if (!(xsave_instruction & BIT(0))) {
                printf("XSAVEOPT requested, but not supported\n");
                return false;
            }
        } else if (config_set(CONFIG_XSAVE_XSAVEC)) {
            if (!(xsave_instruction & BIT(1))) {
                printf("XSAVEC requested, but not supported\n");
                return false;
            }
        } else if (config_set(CONFIG_XSAVE_XSAVES)) {
            if (!(xsave_instruction & BIT(3))) {
                printf("XSAVES requested, but not supported\n");
                return false;
            }
            /* initialize the XSS MSR */
            x86_wrmsr(IA32_XSS_MSR, desired_features);
        }
        /* Load a NULL fpu state so that the idle thread ends up
         * with a sensible FPU state and we can optimize our
         * switch of it */
        memzero(&x86KSnullFpuState, sizeof(x86KSnullFpuState));
        loadFpuState(&x86KSnullFpuState);
    } else {
        /* Store the null fpu state */
        saveFpuState(&x86KSnullFpuState);
    }
    /* Set the FPU to lazy switch mode */
    disableFpu();
    return true;
}
