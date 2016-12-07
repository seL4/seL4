/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#include <config.h>
#include <model/statedata.h>
#include <arch/machine/fpu.h>
#include <kernel/traps.h>
#include <arch/machine/debug.h>

#include <api/syscall.h>

void VISIBLE NORETURN restore_user_context(void)
{
    NODE_UNLOCK_IF_HELD;
    c_exit_hook();
    tcb_t *cur_thread = NODE_STATE(ksCurThread);
    word_t *irqstack = MODE_NODE_STATE(x64KSIRQStack);
    lazyFPURestore(cur_thread);

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(cur_thread);
#endif

#if CONFIG_MAX_NUM_NODES > 1
    cpu_id_t cpu = getCurrentCPUIndex();
    swapgs();
#endif

    /* Now that we have swapped back to the user gs we can safely
     * update the GS base. We must *not* use any kernel functions
     * that rely on having a kernel GS though. Most notably uses
     * of NODE_STATE etc cannot be used beyond this point */
    word_t base = getRegister(cur_thread, TLS_BASE);
    x86_write_fs_base(base, SMP_TERNARY(cpu, 0));

    base = cur_thread->tcbIPCBuffer;
    x86_write_gs_base(base, SMP_TERNARY(cpu, 0));

    // Check if we are returning from a syscall/sysenter or from an interrupt
    // There is a special case where if we would be returning from a sysenter,
    // but are current singlestepping, do a full return like an interrupt
    if (likely(cur_thread->tcbArch.tcbContext.registers[Error] == -1) &&
            (!config_set(CONFIG_SYSENTER) || !config_set(CONFIG_HARDWARE_DEBUG_API) || ((cur_thread->tcbArch.tcbContext.registers[FLAGS] & FLAGS_TF) == 0))) {
        if (config_set(CONFIG_SYSENTER)) {
            cur_thread->tcbArch.tcbContext.registers[FLAGS] &= ~FLAGS_IF;
            asm volatile(
                // Set our stack pointer to the top of the tcb so we can efficiently pop
                "movq %0, %%rsp\n"
                "popq %%rdi\n"
                "popq %%rsi\n"
                "popq %%rax\n"
                "popq %%rbx\n"
                "popq %%rbp\n"
                "popq %%r12\n"
                "popq %%r13\n"
                "popq %%r14\n"
                // skip RDX
                "addq $8, %%rsp\n"
                "popq %%r10\n"
                "popq %%r8\n"
                "popq %%r9\n"
                "popq %%r15\n"
                //restore RFLAGS
                "popfq\n"
                // reset interrupt bit
                "orq %[IF], -8(%%rsp)\n"
                // Restore NextIP
                "popq %%rdx\n"
                // Skip ERROR
                "addq $8, %%rsp\n"
                // Restore RSP
                "popq %%rcx\n"
                // Skip TLS_BASE, FaultIP
                "addq $16, %%rsp\n"
                "popq %%r11\n"
                // More register but we can ignore and are done restoring
                // enable interrupt disabled by sysenter
                "sti\n"
                /* return to user
                 * sysexit with rex.w user code = cs + 32, user data = cs + 40.
                 * without rex.w user code = cs + 16, user data = cs + 24
                 * */
                "rex.w sysexit\n"
                :
                : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI]),
                [IF] "i" (FLAGS_IF)
                // Clobber memory so the compiler is forced to complete all stores
                // before running this assembler
                : "memory"
            );
        } else {
            asm volatile(
                // Set our stack pointer to the top of the tcb so we can efficiently pop
                "movq %0, %%rsp\n"
                "popq %%rdi\n"
                "popq %%rsi\n"
                "popq %%rax\n"
                "popq %%rbx\n"
                "popq %%rbp\n"
                "popq %%r12\n"
                "popq %%r13\n"
                "popq %%r14\n"
                "popq %%rdx\n"
                "popq %%r10\n"
                "popq %%r8\n"
                "popq %%r9\n"
                "popq %%r15\n"
                //restore RFLAGS
                "popq %%r11\n"
                // Restore NextIP
                "popq %%rcx\n"
                // clear RSP to not leak information to the user
                "xor %%rsp, %%rsp\n"
                // More register but we can ignore and are done restoring
                // enable interrupt disabled by sysenter
                "rex.w sysret\n"
                :
                : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI])
                // Clobber memory so the compiler is forced to complete all stores
                // before running this assembler
                : "memory"
            );
        }
    } else {
        /* construct our return from interrupt frame */
        irqstack[1] = getRegister(cur_thread, NextIP);
        irqstack[2] = getRegister(cur_thread, CS);
        irqstack[3] = getRegister(cur_thread, FLAGS);
        irqstack[4] = getRegister(cur_thread, RSP);
        irqstack[5] = getRegister(cur_thread, SS);
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movq %0, %%rsp\n"
            "popq %%rdi\n"
            "popq %%rsi\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            "popq %%rbp\n"
            "popq %%r12\n"
            "popq %%r13\n"
            "popq %%r14\n"
            "popq %%rdx\n"
            "popq %%r10\n"
            "popq %%r8\n"
            "popq %%r9\n"
            "popq %%r15\n"
            /* skip RFLAGS, Error NextIP RSP, TLS_BASE, FaultIP */
            "addq $48, %%rsp\n"
            "popq %%r11\n"
            "popq %%rcx\n"
#if CONFIG_MAX_NUM_NODES > 1
            // Swapping gs twice here is worth it as it allows us to efficiently
            // set the user gs base previously
            "swapgs\n"
            "movq %%gs:8, %%rsp\n"
            "addq $8, %%rsp\n"
            // Switch to the user GS value
            "swapgs\n"
#else
            "leaq x64KSIRQStack + 8, %%rsp\n"
#endif
            "iretq\n"
            :
            : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI])
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }
    UNREACHABLE();
}

void VISIBLE NORETURN c_x64_handle_interrupt(int irq, int syscall);
void VISIBLE NORETURN c_x64_handle_interrupt(int irq, int syscall)
{
    setRegister(NODE_STATE(ksCurThread), Error, MODE_NODE_STATE(x64KSIRQStack)[0]);
    /* In the case of an interrupt the NextIP and the FaultIP should be the same value,
     * i.e. the address of the instruction the CPU was about to execute before the
     * interrupt. This is the 5th value pushed on by the hardware, so indexing from
     * the bottom is x64KSIRQStack[1] */
    setRegister(NODE_STATE(ksCurThread), NextIP, MODE_NODE_STATE(x64KSIRQStack)[1]);
    setRegister(NODE_STATE(ksCurThread), FaultIP, MODE_NODE_STATE(x64KSIRQStack)[1]);
    setRegister(NODE_STATE(ksCurThread), FLAGS, MODE_NODE_STATE(x64KSIRQStack)[3]);
    setRegister(NODE_STATE(ksCurThread), RSP, MODE_NODE_STATE(x64KSIRQStack)[4]);
    c_handle_interrupt(irq, syscall);
    UNREACHABLE();
}
