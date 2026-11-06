/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef ENABLE_SMP_SUPPORT

#include <mode/smp/ipi.h>
#include <smp/ipi.h>
#include <smp/lock.h>

/*
 * Async reschedule request IPIs are handled like any other interrupt with no
 * special treatment.
 *
 * The blocking remote call IPI flow is:
 * - The caller core setups remote call info, sends IPI(s) and blocks till all
 *   targeted cores have handled the call, all while holding the global lock.
 * - Remote core enters kernel mode for any reason, usually because of the IPI
 *   interrupt, but it may race with a syscall, fault or other IRQ.
 * - A woken up core tries to take the global lock, sees the pending IPI and
 *   handles it.
 * - Once done, it notifies the caller by decrementing a counter and continues
 *   blocking on the kernel lock. At this point the IPI interrupt will still be
 *   outstanding, but will be ignored later. It could be cleared earlier, but
 *   that would lead to spurious interrupt warnings.
 * - When all remote cores are done, the caller core finishes the syscall that
 *   needed the remote call and unlocks the global lock.
 * - Then the remote cores will unblock one by one and continue normally.
 *
 * remoteTCBStall() needs special attention: It does a 'IpiRemoteCall_Stall' which
 * gets handled by ipiStallCoreCallback(). It is a request to stop running the
 * current thread because the caller wants to do operations on it. The remote core
 * switches to the idle task, but only if it entered the kernel via an interrupt:
 *
 * In case of a syscall or fault, if it switched to the idle task and continued
 * normally, it would seem like the idle task did a syscall or faulted. We could
 * check for this corner case in all kernel entry paths, or return to user space
 * directly, but getting all details right is tricky.
 *
 * Instead, refuse to do a stall in these cases and let the caller retry later.
 * As all remote core stall requests are triggered via syscalls, those syscalls
 * can be restarted. This is done by returning EXCEPTION_PREEMPTED, because
 * handleInvocation() doesn't honour ThreadState_Restart. The kernel lock is a
 * queued lock, so forward progress is guaranteed.
 */
void ipiStallCoreCallback(void)
{
    assert(NODE_STATE(ksCurThread) != NODE_STATE(ksIdleThread));
    assert(NODE_STATE(ksSchedulerAction) == SchedulerAction_ResumeCurrentThread ||
           NODE_STATE(ksSchedulerAction) == SchedulerAction_ChooseNewThread);

    /* Put the current task at the head of the scheduling queue: */
    SCHED_ENQUEUE_CURRENT_TCB;
    /* Request a schedule, so that we switch away from the idle thread again:  */
    NODE_STATE(ksSchedulerAction) = SchedulerAction_ChooseNewThread;
#ifdef CONFIG_HAVE_FPU
    /* Might save another remote call later: */
    fpuRelease(NODE_STATE(ksCurThread));
#endif
#ifdef CONFIG_ARCH_X86
    arch_c_entry_hook(); // TODO: Remove when x86 saves thread id registers in asm
#endif
    /*
     * This only does a state switch, schedule() will be called before we return
     * to user space.
     */
    switchToIdleThread();
#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurSC) = NODE_STATE(ksIdleSC);
#endif
}

NO_INLINE void ipi_remote_call(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t mask)
{
    word_t cpu = getCurrentCPUIndex();
    ipi_state_t *ipi = &big_kernel_lock.ipi;

    assert(!big_kernel_lock.node[cpu].ipi);
    assert(ipi->call == IpiNumArchRemoteCall);

    /* Make sure the current core is not set in the mask */
    mask &= ~BIT(cpu);
    /* This may happen, e.g. the caller tries to map a pagetable in
     * newly created PD which has not been run yet. Guard against them! */
    if (mask == 0) {
        return;
    }
    /* Get number of cores involved in this IPI */
    ipi->count = popcountl(mask);
    ipi->call = func;
    ipi->retry = false;
    ipi->args[0] = data1;
    ipi->args[1] = data2;
    ipi->args[2] = data3;
    /* Make sure no resource access passes from this point */
    __atomic_thread_fence(__ATOMIC_ACQ_REL);

    /* Send IPIs. This must set node[].ipi for all nodes being called: */
    arch_ipi_send_mask(CORE_IRQ_TO_IRQT(0, irq_remote_call_ipi), mask, true);

    /* Wait for all remote cores: */
    while (__atomic_load_n(&ipi->count, __ATOMIC_ACQUIRE)) {
        arch_pause();
    }
    ipi->call = IpiNumArchRemoteCall;
}

void ipi_handle_call(bool_t irqPath)
{
    word_t cpu = getCurrentCPUIndex();
    clh_node_t *node = &big_kernel_lock.node[cpu];
    ipi_state_t *ipi = &big_kernel_lock.ipi;

    node->ipi = 0;
    /*
     * If we entered the kernel to do a syscall or to handle a fault,
     * we can't handle a stall IPI right now. Tell the remote core to
     * retry later.
     *
     * TODO: On MCS, also retry on timeoutfault?
     */
    if (!irqPath && ipi->call == IpiRemoteCall_Stall) {
        big_kernel_lock.ipi.retry = true;
    } else {
        arch_handleRemoteCall(ipi->call, ipi->args[0], ipi->args[1], ipi->args[2]);
    }
    /* Tell caller core we are done: */
    __atomic_sub_fetch(&ipi->count, 1, __ATOMIC_ACQ_REL);
}

void handleIPI(irq_t irq)
{
    if (IRQT_TO_IRQ(irq) == irq_reschedule_ipi) {
        rescheduleRequired();
    }
}

bool_t doRemoteStall(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_Stall, cpu);
    if (big_kernel_lock.ipi.retry) {
        return false;
    }
    return true;
}

void doMaskReschedule(word_t mask)
{
    /* make sure the current core is not set in the mask */
    mask &= ~BIT(getCurrentCPUIndex());
    if (mask != 0) {
        arch_ipi_send_mask(CORE_IRQ_TO_IRQT(0, irq_reschedule_ipi), mask, false);
    }
}

void generic_ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking)
{
    while (mask) {
        unsigned int core = wordBits - 1 - clzl(mask);

        if (isBlocking) {
            /*
             * All writes before setting ipi to 1 must be observed,
             * as other cores may check the ipi flag at any moment.
             */
            assert(big_kernel_lock.node[core].ipi == 0);
            __atomic_store_n(&big_kernel_lock.node[core].ipi, 1, __ATOMIC_RELEASE);
        }
        arch_ipi_send_target(ipi, cpuIndexToID(core));
        mask &= ~BIT(core);
    }
}

#ifdef CONFIG_DEBUG_BUILD
exception_t handle_SysDebugSendIPI(void)
{
#ifdef CONFIG_ARCH_ARM
    word_t target = getRegister(NODE_STATE(ksCurThread), capRegister);
    word_t irq = getRegister(NODE_STATE(ksCurThread), msgInfoRegister);
    if (target > CONFIG_MAX_NUM_NODES) {
        userError("SysDebugSendIPI: Invalid target, halting");
        halt();
    }
    if (irq > 15) {
        userError("SysDebugSendIPI: Invalid IRQ, not a SGI, halting");
        halt();
    }
    arch_ipi_send_target(CORE_IRQ_TO_IRQT(0, irq), BIT(target));
#else /* not CONFIG_ARCH_ARM */
    userError("SysDebugSendIPI: not supported on this architecture");
    halt();
#endif  /* [not] CONFIG_ARCH_ARM */
    return EXCEPTION_NONE;
}
#endif /* CONFIG_DEBUG_BUILD */

#endif /* ENABLE_SMP_SUPPORT */
