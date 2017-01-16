/*
 * Copyright 2016, Data61 CSIRO
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#include <config.h>
#include <arch/kernel/traps.h>
#include <arch/object/vcpu.h>
#include <arch/machine/registerset.h>
#include <api/syscall.h>

#include <benchmark/benchmark_track_types.h>
#include <benchmark/benchmark_track.h>
#include <benchmark/benchmark_utilisation.h>

void VISIBLE NORETURN
c_handle_undefined_instruction(void)
{
    c_entry_hook();

#ifdef TRACK_KERNEL_ENTRIES
    ksKernelEntry.path = Entry_UserLevelFault;
    ksKernelEntry.word = getRegister(NODE_STATE(ksCurThread), LR_svc);
#endif

    /* There's only one user-level fault on ARM, and the code is (0,0) */
    handleUserLevelFault(0, 0);
    restore_user_context();
    UNREACHABLE();
}

static inline void NORETURN
c_handle_vm_fault(vm_fault_type_t type)
{
    c_entry_hook();

#ifdef TRACK_KERNEL_ENTRIES
    ksKernelEntry.path = Entry_VMFault;
    ksKernelEntry.word = getRegister(NODE_STATE(ksCurThread), LR_svc);
#endif

    handleVMFaultEvent(type);
    restore_user_context();
    UNREACHABLE();
}

void VISIBLE NORETURN
c_handle_data_fault(void)
{
    c_handle_vm_fault(seL4_DataFault);
}

void VISIBLE NORETURN
c_handle_instruction_fault(void)
{
    c_handle_vm_fault(seL4_InstructionFault);
}

void VISIBLE NORETURN
c_handle_interrupt(void)
{
    c_entry_hook();

#ifdef TRACK_KERNEL_ENTRIES
    ksKernelEntry.path = Entry_Interrupt;
    ksKernelEntry.word = getActiveIRQ();
#endif

    handleInterruptEntry();
    restore_user_context();
}

void NORETURN
slowpath(syscall_t syscall)
{
#ifdef TRACK_KERNEL_ENTRIES
    ksKernelEntry.is_fastpath = 0;
#endif /* TRACK KERNEL ENTRIES */
    handleSyscall(syscall);

    restore_user_context();
    UNREACHABLE();
}

void VISIBLE
c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
{
    c_entry_hook();
#if TRACK_KERNEL_ENTRIES
    benchmark_debug_syscall_start(cptr, msgInfo, syscall);
    ksKernelEntry.is_fastpath = 1;
#endif /* DEBUG */

#ifdef CONFIG_FASTPATH
    if (syscall == SysCall) {
        fastpath_call(cptr, msgInfo);
        UNREACHABLE();
    } else if (syscall == SysReplyRecv) {
        fastpath_reply_recv(cptr, msgInfo);
        UNREACHABLE();
    }
#endif /* CONFIG_FASTPATH */

    if (unlikely(syscall < SYSCALL_MIN || syscall > SYSCALL_MAX)) {
#ifdef TRACK_KERNEL_ENTRIES
        ksKernelEntry.path = Entry_UnknownSyscall;
        /* ksKernelEntry.word word is already set to syscall */
#endif /* TRACK_KERNEL_ENTRIES */
        handleUnknownSyscall(syscall);
        restore_user_context();
        UNREACHABLE();
    } else {
        slowpath(syscall);
        UNREACHABLE();
    }
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
VISIBLE NORETURN void
c_handle_vcpu_fault(word_t hsr)
{
    c_entry_hook();

#ifdef TRACK_KERNEL_ENTRIES
    ksKernelEntry.path = Entry_VCPUFault;
    ksKernelEntry.word = hsr;
#endif
    handleVCPUFault(hsr);
    restore_user_context();
    UNREACHABLE();
}
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
