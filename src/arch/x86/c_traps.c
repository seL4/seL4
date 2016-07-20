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
#include <arch/kernel/traps.h>

#include <api/syscall.h>

void NORETURN VISIBLE slowpath_irq(irq_t irq);
void NORETURN VISIBLE restore_user_context(void);

void VISIBLE c_handle_interrupt(int irq, int syscall);
void VISIBLE c_handle_interrupt(int irq, int syscall)
{
    if (irq == int_unimpl_dev) {
        handleUnimplementedDevice();
    } else if (irq == int_page_fault) {
        /* Error code is in Error. Pull out bit 5, which is whether it was instruction or data */
        handleVMFaultEvent((ksCurThread->tcbArch.tcbContext.registers[Error] >> 4) & 1);
    } else if (irq < int_irq_min) {
        handleUserLevelFault(irq, ksCurThread->tcbArch.tcbContext.registers[Error]);
    } else if (likely(irq < int_trap_min)) {
        x86KScurInterrupt = irq;
        fastpath_irq();
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
        handleUnknownSyscall(sys_num);
    }
    restore_user_context();
}

void NORETURN
slowpath_irq(irq_t irq)
{
    handleInterruptEntry(irq);
    restore_user_context();
}

void NORETURN
slowpath(syscall_t syscall)
{
    x86KScurInterrupt = -1;
    /* increment NextIP to skip sysenter */
    /* check for undefined syscall */
    if (unlikely(syscall < SYSCALL_MIN || syscall > SYSCALL_MAX)) {
        ksCurThread->tcbArch.tcbContext.registers[FaultIP] = ksCurThread->tcbArch.tcbContext.registers[NextIP];
        ksCurThread->tcbArch.tcbContext.registers[NextIP] += 2;
        handleUnknownSyscall(syscall);
    } else {
        ksCurThread->tcbArch.tcbContext.registers[NextIP] += 2;
        handleSyscall(syscall);
    }
    restore_user_context();
}

void VISIBLE c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
{
#ifdef CONFIG_FASTPATH
    if (syscall == SysCall) {
        fastpath_call(cptr, msgInfo);
        UNREACHABLE();
    } else if (syscall == SysReplyRecv) {
        fastpath_reply_recv(cptr, msgInfo);
        UNREACHABLE();
    } else if (syscall == SysSend) {
        fastpath_signal(cptr);
        UNREACHABLE();
    }
#endif /* CONFIG_FASTPATH */

    slowpath(syscall);
}
