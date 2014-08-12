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
#include <arch/fastpath/fastpath.h>

#include <api/syscall.h>

void __attribute__((noreturn)) __attribute__((externally_visible)) restore_user_context(void);
void __attribute__((noreturn)) __attribute__((externally_visible)) restore_user_context(void)
{
    /* set the tss.esp0 */
    tss_ptr_set_esp0(&ia32KStss, ((uint32_t)ksCurThread) + 0x4c);
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
    if (likely(ksCurThread->tcbContext.registers[Error] == -1)) {
        ksCurThread->tcbContext.registers[EFLAGS] &= ~0x200;
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movl %0, %%esp\n"
            // restore syscall number
            "popl %%eax\n"
            // cap/badge reigster
            "popl %%ebx\n"
            // skip ecx and edx, these will contain esp and nexteip due to sysenter/sysexit convention
            "addl $8, %%esp\n"
            // message info register
            "popl %%esi\n"
            // message register
            "popl %%edi\n"
            // message register
            "popl %%ebp\n"
            //ds (if changed)
            "cmpl $0x23, (%%esp)\n"
            "je 1f\n"
            "popl %%ds\n"
            "jmp 2f\n"
            "1: addl $4, %%esp\n"
            "2:\n"
            //es (if changed)
            "cmpl $0x23, (%%esp)\n"
            "je 1f\n"
            "popl %%es\n"
            "jmp 2f\n"
            "1: addl $4, %%esp\n"
            "2:\n"
            //have to reload other selectors
            "popl %%fs\n"
            "popl %%gs\n"
            // skip faulteip, tls_base and error (these are fake registers)
            "addl $12, %%esp\n"
            // restore nexteip
            "popl %%edx\n"
            // skip cs
            "addl $4,  %%esp\n"
            "popfl\n"
            // reset interrupt bit
            "orl $0x200, -4(%%esp)\n"
            // restore esp
            "pop %%ecx\n"
            "sti\n"
            "sysexit\n"
            :
            : "r"(&ksCurThread->tcbContext.registers[EAX])
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    } else {
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movl %0, %%esp\n"
            "popl %%eax\n"
            "popl %%ebx\n"
            "popl %%ecx\n"
            "popl %%edx\n"
            "popl %%esi\n"
            "popl %%edi\n"
            "popl %%ebp\n"
            "popl %%ds\n"
            "popl %%es\n"
            "popl %%fs\n"
            "popl %%gs\n"
            // skip faulteip, tls_base, error
            "addl $12, %%esp\n"
            "iret\n"
            :
            : "r"(&ksCurThread->tcbContext.registers[EAX])
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }
    while (1);
}

void __attribute__((fastcall)) __attribute__((externally_visible)) c_handle_interrupt(int irq, int syscall);
void __attribute__((fastcall)) __attribute__((externally_visible)) c_handle_interrupt(int irq, int syscall)
{
    if (irq == int_unimpl_dev) {
        handleUnimplementedDevice();
    } else if (irq == int_page_fault) {
        /* Error code is in Error. Pull out bit 5, which is whether it was instruction or data */
        handleVMFaultEvent((ksCurThread->tcbContext.registers[Error] >> 4) & 1);
    } else if (irq < int_irq_min) {
        handleUserLevelFault(irq, ksCurThread->tcbContext.registers[Error]);
    } else if (likely(irq < int_trap_min)) {
        ia32KScurInterrupt = irq;
        handleInterruptEntry();
    } else if (irq == int_spurious) {
        /* fall through to restore_user_context and do nothing */
    } else {
        /* Interpret a trap as an unknown syscall */
        /* Adjust FaultEIP to point to trapping INT
         * instruction by subtracting 2 */
        int sys_num;
        ksCurThread->tcbContext.registers[FaultEIP] -= 2;
        /* trap number is MSBs of the syscall number and the LSBS of EAX */
        sys_num = (irq << 24) | (syscall & 0x00ffffff);
        handleUnknownSyscall(sys_num);
    }
    restore_user_context();
}

void __attribute__((noreturn))
slowpath(syscall_t syscall)
{
    ia32KScurInterrupt = -1;
    /* increment nextEIP to skip sysenter */
    ksCurThread->tcbContext.registers[NextEIP] += 2;
    /* check for undefined syscall */
    if (unlikely(syscall < SYSCALL_MIN || syscall > SYSCALL_MAX)) {
        handleUnknownSyscall(syscall);
    } else {
        handleSyscall(syscall);
    }
    restore_user_context();
}

void __attribute__((externally_visible)) c_handle_syscall(syscall_t syscall, word_t cptr, word_t msgInfo);
void __attribute__((externally_visible)) c_handle_syscall(syscall_t syscall, word_t cptr, word_t msgInfo)
{
#ifdef FASTPATH
    if (syscall == SysCall) {
        fastpath_call(cptr, msgInfo);
    } else if (syscall == SysReplyWait) {
        fastpath_reply_wait(cptr, msgInfo);
    }
#endif

    slowpath(syscall);
}
