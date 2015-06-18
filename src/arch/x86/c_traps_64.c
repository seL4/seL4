/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <model/statedata.h>
#include <arch/kernel/lock.h>
#include <arch/machine/fpu.h>

#include <api/syscall.h>

void __attribute__((noreturn)) __attribute__((externally_visible)) restore_user_context(void);
void __attribute__((noreturn)) __attribute__((externally_visible)) restore_user_context(void)
{
    /* set the tss.rsp0. Needs to be set in two parts as it crosses a word boundary
     * as a 64-bit value */
    word_t context_base = ((word_t)ksCurThread) + (n_contextRegisters * sizeof(word_t));
    tss_ptr_set_rsp0_l(&x86KStss, (uint32_t)context_base);
    tss_ptr_set_rsp0_u(&x86KStss, (uint32_t)(context_base >> 32));
    if (unlikely(ksCurThread == ia32KSfpuOwner)) {
        /* We are using the FPU, make sure it is enabled */
        enableFpu();
    } else if (unlikely(ia32KSfpuOwner)) {
        /* Someone is using the FPU and it might be enabled */
        disableFpu();
    } else {
        /* No-one (including us) is using the FPU, so we assume it
         * is currently disabled */
    }
    /* see if we entered via syscall */
    if (likely(ksCurThread->tcbArch.tcbContext.registers[Error] == -1)) {
        ksCurThread->tcbArch.tcbContext.registers[RFLAGS] &= ~0x200;
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movq %0, %%rsp\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            // skip RCX and RDX
            "addq $16, %%rsp\n"
            "popq %%rsi\n"
            "popq %%rdi\n"
            "popq %%rbp\n"
            "popq %%r8\n"
            "popq %%r9\n"
            "popq %%r10\n"
            "popq %%r11\n"
            "popq %%r12\n"
            "popq %%r13\n"
            "popq %%r14\n"
            "popq %%r15\n"
            // skip DS, ES, FS, GS, FaultIP, TLS_BASE, padding, Error
            "addq $64, %%rsp\n"
            // Restore NextIP
            "popq %%rdx\n"
            // skip CS
            "addq $8, %%rsp\n"
            //restore RFLAGS
            "popfq\n"
            // reset interrupt bit
            "orq $0x200, -8(%%rsp)\n"
            // restore RSP
            "popq %%rcx\n"
            // SS is next but we can ignore it and are done restoring registers
            // enable interrupt disabled by sysenter
            "sti\n"
            /* return to user
             * sysexit with rex.w user code = cs + 32, user data = cs + 40.
             * without rex.w user code = cs + 16, user data = cs + 24 
             * */
            "rex.w sysexit\n"
            :
            : "r"(&ksCurThread->tcbArch.tcbContext.registers[RAX])
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    } else {
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movq %0, %%rsp\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            "popq %%rcx\n"
            "popq %%rdx\n"
            "popq %%rsi\n"
            "popq %%rdi\n"
            "popq %%rbp\n"
            "popq %%r8\n"
            "popq %%r9\n"
            "popq %%r10\n"
            "popq %%r11\n"
            "popq %%r12\n"
            "popq %%r13\n"
            "popq %%r14\n"
            "popq %%r15\n"
            // skip DS, ES, FS, GS, FaultRIP, TLS_BASE, padding, and error code
            "addq $64, %%rsp\n"
            "iretq\n"
            :
            : "r"(&ksCurThread->tcbArch.tcbContext.registers[RAX])
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }
    while(1);
}
