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
    context->fpuState = ia32KSnullFpuState;
}

/*
 * Switch the owner of the FPU to the given thread.
 */
static void
switchFpuOwner(tcb_t *new_owner)
{
    enableFpu();
    if (ia32KSfpuOwner) {
        saveFpuState(&ia32KSfpuOwner->tcbContext.fpuState);
    }
    if (new_owner) {
        loadFpuState(&new_owner->tcbContext.fpuState);
    } else {
        disableFpu();
    }
    ia32KSfpuOwner = new_owner;
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
    assert(ksCurThread != ia32KSfpuOwner);

    /* Otherwise, lazily switch over the FPU. */
    switchFpuOwner(ksCurThread);

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
     * so that 'ia32KSfpuOwner' doesn't point to invalid memory.
     */
    if (ia32KSfpuOwner == thread) {
        switchFpuOwner(NULL);
    }
}

/*
 * Initialise the FPU for this machine.
 */
BOOT_CODE void
Arch_initFpu(void)
{
    /* Enable FPU / SSE / SSE2 / SSE3 / SSSE3 / SSE4 Extensions. */
    write_cr4(read_cr4() | CR4_OSFXSR);

    /* Enable the FPU in general. Although leave it in a state where it will
     * generate a fault if someone tries to use it as we implement lazy
     * switching */
    write_cr0((read_cr0() & ~CR0_EMULATION) | CR0_MONITOR_COPROC | CR0_NUMERIC_ERROR | CR0_TASK_SWITCH);
}
