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
#include <kernel/stack.h>
#include <machine/fpu.h>
#include <arch/fastpath/fastpath.h>
#include <arch/machine/debug.h>
#include <benchmark/benchmark_track.h>
#include <mode/stack.h>
#include <arch/object/vcpu.h>
#include <arch/kernel/traps.h>

#include <api/syscall.h>
#include <util.h>

#ifdef CONFIG_VTX
static void NORETURN vmlaunch_failed(void)
{
    NODE_LOCK_SYS;

    c_entry_hook();

    handleVmEntryFail();
    restore_user_context();
}

static void NORETURN restore_vmx(void)
{
    restoreVMCS();
#ifdef CONFIG_HARDWARE_DEBUG_API
    /* Do not support breakpoints in VMs, so just disable all breakpoints */
    loadAllDisabledBreakpointState(ksCurThread);
#endif
#ifdef ENABLE_SMP_SUPPORT
    NODE_STATE(ksCurThread)->tcbArch.tcbVCPU->kernelSP = ((word_t)kernel_stack_alloc[getCurrentCPUIndex()]) + BIT(CONFIG_KERNEL_STACK_BITS) - 4;
#endif /* ENABLE_SMP_SUPPORT */
    if (NODE_STATE(ksCurThread)->tcbArch.tcbVCPU->launched) {
        /* attempt to do a vmresume */
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
            // Now do the vmresume
            "vmresume\n"
            // if we get here we failed
#ifdef ENABLE_SMP_SUPPORT
            "movl (%%esp), %%esp\n"
#else
            "leal kernel_stack_alloc + %c2, %%esp\n"
#endif
            "call %1\n"
            :
            : "r"(&NODE_STATE(ksCurThread)->tcbArch.tcbVCPU->gp_registers[VCPU_EAX]),
            "m"(vmlaunch_failed),
            "i"(BIT(CONFIG_KERNEL_STACK_BITS) - sizeof(word_t))
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    } else {
        /* attempt to do a vmlaunch */
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
            // Now do the vmresume
            "vmlaunch\n"
            // if we get here we failed
#ifdef ENABLE_SMP_SUPPORT
            "movl (%%esp), %%esp\n"
#else
            "leal kernel_stack_alloc + %c2, %%esp\n"
#endif
            "call %1\n"
            :
            : "r"(&NODE_STATE(ksCurThread)->tcbArch.tcbVCPU->gp_registers[VCPU_EAX]),
            "m"(vmlaunch_failed),
            "i"(BIT(CONFIG_KERNEL_STACK_BITS) - sizeof(word_t))
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }
    UNREACHABLE();
}
#endif

void NORETURN VISIBLE restore_user_context(void);
void NORETURN VISIBLE restore_user_context(void)
{
    c_exit_hook();

    NODE_UNLOCK_IF_HELD;

    /* we've now 'exited' the kernel. If we have a pending interrupt
     * we should 'enter' it again */
    if (ARCH_NODE_STATE(x86KSPendingInterrupt) != int_invalid) {
        /* put this in service */
        interrupt_t irq = servicePendingIRQ();
        /* reset our stack and jmp to the IRQ entry point */
        asm volatile(
            "mov %[stack_top], %%esp\n"
            "push %[syscall] \n"
            "push %[irq]\n"
            "call c_handle_interrupt"
            :
            : [stack_top] "r"(&(kernel_stack_alloc[CURRENT_CPU_INDEX()][BIT(CONFIG_KERNEL_STACK_BITS)])),
            [syscall] "r"(0), /* syscall is unused for irq path */
            [irq] "r"(irq)
            : "memory");
        UNREACHABLE();
    }

#ifdef CONFIG_VTX
    if (thread_state_ptr_get_tsType(&NODE_STATE(ksCurThread)->tcbState) == ThreadState_RunningVM) {
        restore_vmx();
    }
#endif
    setKernelEntryStackPointer(NODE_STATE(ksCurThread));
    lazyFPURestore(NODE_STATE(ksCurThread));

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(NODE_STATE(ksCurThread));
#endif

    word_t base = getRegister(NODE_STATE(ksCurThread), TLS_BASE);
    x86_write_gs_base(base, SMP_TERNARY(getCurrentCPUIndex(), 0));

    base = NODE_STATE(ksCurThread)->tcbIPCBuffer;
    x86_write_fs_base(base, SMP_TERNARY(getCurrentCPUIndex(), 0));

    if (config_set(CONFIG_KERNEL_X86_IBRS_BASIC)) {
        x86_disable_ibrs();
    }

    /* see if we entered via syscall */
    if (likely(NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[Error] == -1)) {
        NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[FLAGS] &= ~FLAGS_IF;
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
            "movl 4(%%esp), %%ecx\n"
            "popfl\n"
            "orl %[IFMASK], -4(%%esp)\n"
            "sti\n"
            "sysexit\n"
            :
            : "r"(NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers),
            [IFMASK]"i"(FLAGS_IF)
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
            : "r"(NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers)
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }

    UNREACHABLE();
}
