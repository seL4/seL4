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

extern word_t irq_stack[6];

void VISIBLE NORETURN restore_user_context(void)
{
    c_exit_hook();
    if (unlikely(nativeThreadUsingFPU(NODE_STATE(ksCurThread)))) {
        /* We are using the FPU, make sure it is enabled */
        enableFpu();
    } else if (unlikely(ARCH_NODE_STATE(x86KSActiveFPUState))) {
        /* Someone is using the FPU and it might be enabled */
        disableFpu();
    } else {
        /* No-one (including us) is using the FPU, so we assume it
         * is currently disabled */
    }

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(ksCurThread);
#endif

    // Check if we are returning from a syscall/sysenter or from an interrupt
    // There is a special case where if we would be returning from a sysenter,
    // but are current singlestepping, do a full return like an interrupt
    if (likely(ksCurThread->tcbArch.tcbContext.registers[Error] == -1) &&
            (!config_set(CONFIG_SYSENTER) || !config_set(CONFIG_HARDWARE_DEBUG_API) || ((ksCurThread->tcbArch.tcbContext.registers[FLAGS] & FLAGS_TF) == 0))) {
        if (config_set(CONFIG_SYSENTER)) {
            ksCurThread->tcbArch.tcbContext.registers[FLAGS] &= ~FLAGS_IF;
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
                "popq %%r15\n"
                // skip RDX
                "addq $8, %%rsp\n"
                "popq %%r10\n"
                "popq %%r8\n"
                "popq %%r9\n"
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
                : "r"(&ksCurThread->tcbArch.tcbContext.registers[RDI]),
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
                "popq %%r15\n"
                "popq %%rdx\n"
                "popq %%r10\n"
                "popq %%r8\n"
                "popq %%r9\n"
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
                : "r"(&ksCurThread->tcbArch.tcbContext.registers[RDI])
                // Clobber memory so the compiler is forced to complete all stores
                // before running this assembler
                : "memory"
            );
        }
    } else {
        /* construct our return from interrupt frame */
        irq_stack[1] = getRegister(ksCurThread, NextIP);
        irq_stack[2] = getRegister(ksCurThread, CS);
        irq_stack[3] = getRegister(ksCurThread, FLAGS);
        irq_stack[4] = getRegister(ksCurThread, RSP);
        irq_stack[5] = getRegister(ksCurThread, SS);
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
            "popq %%r15\n"
            "popq %%rdx\n"
            "popq %%r10\n"
            "popq %%r8\n"
            "popq %%r9\n"
            /* skip RFLAGS, Error NextIP RSP, TLS_BASE, FaultIP */
            "addq $48, %%rsp\n"
            "popq %%r11\n"
            "popq %%rcx\n"
            "leaq irq_stack + 8, %%rsp\n"
            "iretq\n"
            :
            : "r"(&ksCurThread->tcbArch.tcbContext.registers[RDI])
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
    setRegister(ksCurThread, Error, irq_stack[0]);
    setRegister(ksCurThread, NextIP, irq_stack[1]);
    setRegister(ksCurThread, FaultIP, irq_stack[1]);
    setRegister(ksCurThread, FLAGS, irq_stack[3]);
    setRegister(ksCurThread, RSP, irq_stack[4]);
    c_handle_interrupt(irq, syscall);
    UNREACHABLE();
}
