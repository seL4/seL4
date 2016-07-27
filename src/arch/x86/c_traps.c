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
#include <arch/machine/fpu.h>
#include <arch/fastpath/fastpath.h>
#include <arch/kernel/traps.h>
#include <api/syscall.h>

#include <benchmark_track.h>
#include <benchmark_utilisation.h>

void VISIBLE c_handle_interrupt(int irq, int syscall)
{
    /* for annoying reasons we cannot currently consider this the C entry
     * point as we would like to call various functions that are considered
     * as entry points from the point of view of the arm code. Therefore
     * we are *not* calling c_entry_hook here */

    if (irq == int_unimpl_dev) {
        handleUnimplementedDevice();
    } else if (irq == int_page_fault) {
        /* Error code is in Error. Pull out bit 5, which is whether it was instruction or data */
        handleVMFaultEvent((ksCurThread->tcbArch.tcbContext.registers[Error] >> 4) & 1);
    } else if (irq < int_irq_min) {
        handleUserLevelFault(irq, ksCurThread->tcbArch.tcbContext.registers[Error]);
    } else if (likely(irq < int_trap_min)) {
        x86KScurInterrupt = irq;
        handleInterruptEntry();
    } else if (irq == int_spurious) {
        /* fall through to restore_user_context and do nothing */
    } else {
        /* Interpret a trap as an unknown syscall */
        /* Adjust FaultIP to point to trapping INT
         * instruction by subtracting 2 */
        int sys_num;
        ksCurThread->tcbArch.tcbContext.registers[FaultIP] -= 2;
        /* trap number is MSBs of the syscall number and the LSBS of EAX */
        sys_num = (irq << 24) | (syscall & 0x00ffffff);
        /* in this case we are calling a function that is *not*
         * an entry point */
        c_entry_hook();
        handleUnknownSyscall(sys_num);
    }
    restore_user_context();
}

void NORETURN
slowpath(syscall_t syscall)
{
    x86KScurInterrupt = -1;
    /* increment NextIP to skip sysenter */
    ksCurThread->tcbArch.tcbContext.registers[NextIP] += 2;
    /* check for undefined syscall */
    if (unlikely(syscall < SYSCALL_MIN || syscall > SYSCALL_MAX)) {
        handleUnknownSyscall(syscall);
    } else {
        handleSyscall(syscall);
    }

    restore_user_context();
}

void VISIBLE c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
{
    c_entry_hook();

#if defined(DEBUG) || defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES)
    benchmark_debug_syscall_start(cptr, msgInfo, syscall);
#endif /* DEBUG */

#if defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES) || defined(CONFIG_BENCHMARK_TRACK_UTILISATION)
    ksEnter = timestamp();
#endif

#ifdef CONFIG_FASTPATH
    if (syscall == SysCall) {
        fastpath_call(cptr, msgInfo);
        UNREACHABLE();
    } else if (syscall == SysReplyRecv) {
        fastpath_reply_recv(cptr, msgInfo);
        UNREACHABLE();
    }
#endif /* CONFIG_FASTPATH */

    slowpath(syscall);
}
