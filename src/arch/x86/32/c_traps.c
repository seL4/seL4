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
#include <util.h>

void NORETURN VISIBLE restore_user_context(void);
void NORETURN VISIBLE restore_user_context(void)
{
    /* set the tss.esp0 */
    tss_ptr_set_esp0(&x86KStss, ((uint32_t)ksCurThread) + (n_contextRegisters * sizeof(word_t)));
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
        ksCurThread->tcbArch.tcbContext.registers[EFLAGS] &= ~0x200;
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movl %0, %%esp\n"
            // restore syscall number
            "popl %%eax\n"
            // cap/badge register
            "popl %%ebx\n"
            // skip ecx and edx, these will contain esp and NextIP due to sysenter/sysexit convention
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
#if defined(CONFIG_FSGSBASE_GDT)
            //have to reload other selectors
            "popl %%fs\n"
            "popl %%gs\n"
            // skip FaultIP, tls_base and error (these are fake registers)
            "addl $12, %%esp\n"
#elif defined(CONFIG_FSGSBASE_MSR)
            /* FS and GS are not touched if MSRs are used */
            "addl $20, %%esp\n"
#else
#error "Invalid method to set IPCBUF/TLS"
#endif
            // restore NextIP
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
            : "r"(&ksCurThread->tcbArch.tcbContext.registers[EAX])
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
#if defined(CONFIG_FSGSBASE_GDT)
            "popl %%fs\n"
            "popl %%gs\n"
            // skip FaultIP, tls_base, error
            "addl $12, %%esp\n"
#elif defined(CONFIG_FSGSBASE_MSR)
            /* skip fs,gs, faultip tls_base, error */
            "addl $20, %%esp\n"
#else
#error "Invalid method to set IPCBUF/TLS"
#endif
            "iret\n"
            :
            : "r"(&ksCurThread->tcbArch.tcbContext.registers[EAX])
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }
    while (1);
}
