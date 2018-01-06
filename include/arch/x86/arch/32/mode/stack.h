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

#ifndef __ARCH_MODE_STACK_H_
#define __ARCH_MODE_STACK_H_

#include <config.h>
#include <mode/model/smp.h>
#include <mode/machine.h>
#include <kernel/stack.h>

/** Hardware stack switching on exception/IRQ entry.
 *
 * We need to tell the CPU where the TCB register context structure is so it
 * can push to it on entry.
 * @param target_thread The thread we're about to switch to.
 */
static inline void
setKernelEntryStackPointer(tcb_t *target_thread)
{
    word_t register_context_top;
    SMP_COND_STATEMENT(word_t kernel_stack_top);

    /* Update both the TSS and the IA32_SYSENTER_ESP MSR, because both are used.
     *
     * The stack pointer is loaded from the TSS on IRQ and exception entry.
     * The IA32_SYSENTER_ESP MSR is used on syscall entry when SYSENTER is used.
     *
     * For an SMP build, we also have to set the location of the kernel stack for the
     * current CPU, because we use per-CPU stacks.
     */
    /* save kernel stack pointer for next exception */
    SMP_COND_STATEMENT(kernel_stack_top = ((word_t)kernel_stack_alloc[getCurrentCPUIndex()]) + BIT(CONFIG_KERNEL_STACK_BITS) - 4);
    SMP_COND_STATEMENT(NODE_STATE(ksCurThread)->tcbArch.tcbContext.kernelSP = kernel_stack_top);

    register_context_top = (word_t)&target_thread->tcbArch.tcbContext.registers[n_contextRegisters];

    tss_ptr_set_esp0(&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KStss.tss, register_context_top);

    if (config_set(CONFIG_HARDWARE_DEBUG_API)) {
        x86_wrmsr(IA32_SYSENTER_ESP_MSR, register_context_top);
    }
}

#endif /* __ARCH_MODE_STACK_H_ */
