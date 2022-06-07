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

/* This function switches the core it is called on to the idle thread,
 * in order to avoid IPI storms. If the core is waiting on the lock, the actual
 * switch will not occur until the core attempts to obtain the lock, at which
 * point the core will capture the pending IPI, which is discarded.

 * The core who triggered the store is responsible for triggering a reschedule,
 * or this call will idle forever */
void ipiStallCoreCallback(bool_t irqPath)
{
    // ipiStallCore can only be called for a single core at a time to be able to assume
    // kernel mutual exclusion as the caller node is blocked until this stall completes
    // and all other nodes are out of the kernel or blocked on the lock.
    assert(big_kernel_lock.ipi.totalCoreBarrier == 1);

    word_t cpu = getCurrentCPUIndex();
    clh_node_t *node = &big_kernel_lock.node[cpu];

    // There's 3 ways that this function can be called:
    // - On the IRQ path where the lock isn't acquired
    // - On the IRQ path where the lock is attempted to be acquired
    // - On the non-IRQ path where the lock is attempted to be acquired
    // !irqPath implies clh_is_self_in_queue()
    assert(clh_is_self_in_queue() || irqPath);

    /* The current thread is running as we would replace this thread with an idle thread
     *
     * The instruction should be re-executed if we are in kernel to handle syscalls.
     * Also, thread in 'ThreadState_RunningVM' should remain in same state.
     * Note that, 'ThreadState_Restart' does not always result in regenerating exception
     * if we are in kernel to handle them, e.g. hardware single step exception. */
    if (!irqPath && thread_state_ptr_get_tsType(&NODE_STATE(ksCurThread)->tcbState) == ThreadState_Running) {
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    }
#ifdef CONFIG_KERNEL_MCS
    updateTimestamp();
#endif
    if (config_ternary(CONFIG_KERNEL_MCS, checkBudget(),1)) {
        SCHED_ENQUEUE_CURRENT_TCB;
    }
    switchToIdleThread();
    NODE_STATE(ksSchedulerAction) = SchedulerAction_ResumeCurrentThread;
    doMaskReschedule(ARCH_NODE_STATE(ipiReschedulePending));
    ARCH_NODE_STATE(ipiReschedulePending) = 0;

#ifdef CONFIG_KERNEL_MCS
    switchSchedContext();
    NODE_STATE(ksReprogram) = false;
#endif

    if (irqPath) {
        return;
    }

    /* Let the cpu requesting this IPI to continue while we waiting on lock */
    big_kernel_lock.node_owners[getCurrentCPUIndex()].ipi = 0;
#ifdef CONFIG_ARCH_RISCV
    ipi_clear_irq(irq_remote_call_ipi);
#endif
    ipi_wait(totalCoreBarrier);

    /* Continue waiting on lock */
    while (node->watch->state != CLHState_Granted) {
        __atomic_thread_fence(__ATOMIC_ACQUIRE);
        if (clh_is_ipi_pending(cpu)) {
            /* Multiple calls for similar reason could result in stack overflow */
            assert(big_kernel_lock.ipi.remoteCall != IpiRemoteCall_Stall);
            handleIPI(CORE_IRQ_TO_IRQT(cpu, irq_remote_call_ipi), irqPath);
        }
        arch_pause();
    }

    /* make sure no resource access passes from this point */
    __atomic_thread_fence(__ATOMIC_ACQUIRE);

    /* Start idle thread to capture the pending IPI */
    activateThread();
    restore_user_context();
    // No return from restore_user_context()
}

void ipi_wait(void)
{
    ipi_state_t *ipi = &big_kernel_lock.ipi;
    word_t cores = ipi->totalCoreBarrier;
    word_t localsense = ipi->globalsense;
    word_t *count = &ipi->count;

    if (__atomic_fetch_add(count, 1, __ATOMIC_ACQ_REL) == cores) {
        *count = 0;
        ipi->globalsense++;
        __atomic_thread_fence(__ATOMIC_RELEASE);
    }
    /* Check globalsense instead of count to protect against a race where
     * a new IPI started before this core saw that the old one finished. */
    while (localsense == ipi->globalsense) {
        __atomic_thread_fence(__ATOMIC_ACQUIRE);
        arch_pause();
    }
}

static inline void init_ipi_args(IpiRemoteCall_t func,
                                 word_t data1, word_t data2, word_t data3,
                                 word_t mask)
{
    ipi_state_t *ipi = &big_kernel_lock.ipi;

    ipi->remoteCall = func;
    ipi->args[0] = data1;
    ipi->args[1] = data2;
    ipi->args[2] = data3;

    /* get number of cores involved in this IPI */
    ipi->totalCoreBarrier = popcountl(mask);
}

void handleIPI(irq_t irq, bool_t irqPath)
{
    ipi_state_t *ipi = &big_kernel_lock.ipi;

    if (IRQT_TO_IRQ(irq) == irq_remote_call_ipi) {
        handleRemoteCall(ipi->remoteCall, ipi->args[0], ipi->args[1], ipi->args[2], irqPath);
        if (irqPath) {
#ifdef CONFIG_KERNEL_MCS
            assert(!NODE_STATE(ksCurThread)->tcbYieldTo);
            assert(NODE_STATE(ksReprogram) == false);
#endif
            assert(NODE_STATE(ksSchedulerAction) == SchedulerAction_ResumeCurrentThread);
            assert(ARCH_NODE_STATE(ipiReschedulePending) == 0);
            switch (thread_state_get_tsType(NODE_STATE(ksCurThread)->tcbState)) {
                case ThreadState_Running:
                case ThreadState_IdleThreadState:
            #ifdef CONFIG_VTX
                case ThreadState_RunningVM:
            #endif
            break;
            default:
                fail("Current thread is blocked");

            }
        }
    } else if (IRQT_TO_IRQ(irq) == irq_reschedule_ipi) {
        rescheduleRequired();
#ifdef CONFIG_ARCH_RISCV
        ifence_local();
#endif
    } else {
        fail("Invalid IPI");
    }
}

void doRemoteMaskOp(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t mask)
{
    /* make sure the current core is not set in the mask */
    mask &= ~BIT(getCurrentCPUIndex());

    /* this may happen, e.g. the caller tries to map a pagetable in
     * newly created PD which has not been run yet. Guard against them! */
    if (mask != 0) {
        init_ipi_args(func, data1, data2, data3, mask);

        /* make sure no resource access passes from this point */
        asm volatile("" ::: "memory");
        ipi_send_mask(CORE_IRQ_TO_IRQT(0, irq_remote_call_ipi), mask, true);
        ipi_wait();
    }
}

void doMaskReschedule(word_t mask)
{
    /* make sure the current core is not set in the mask */
    mask &= ~BIT(getCurrentCPUIndex());
    if (mask != 0) {
        ipi_send_mask(CORE_IRQ_TO_IRQT(0, irq_reschedule_ipi), mask, false);
    }
}

void generic_ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking)
{
    word_t nr_target_cores = 0;
    uint16_t target_cores[CONFIG_MAX_NUM_NODES];

    while (mask) {
        int index = wordBits - 1 - clzl(mask);
        if (isBlocking) {
            /*
             * All writes before setting ipi to 1 must be observed,
             * as other cores may check the ipi flag at any moment.
             * IPI_MEM_BARRIER is too late to prevent reordering
             * between IPI data and flag reads.
             */
            __atomic_store_n(&big_kernel_lock.node[index].ipi, 1, __ATOMIC_RELEASE);
            target_cores[nr_target_cores] = index;
            nr_target_cores++;
        } else {
            IPI_MEM_BARRIER;
            ipi_send_target(ipi, cpuIndexToID(index));
        }
        mask &= ~BIT(index);
    }

    if (nr_target_cores > 0) {
        /* sending IPIs... */
        IPI_MEM_BARRIER;
        for (int i = 0; i < nr_target_cores; i++) {
            ipi_send_target(ipi, cpuIndexToID(target_cores[i]));
        }
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
    ipi_send_target(CORE_IRQ_TO_IRQT(0, irq), BIT(target));
    return EXCEPTION_NONE;
#else /* not CONFIG_ARCH_ARM */
    userError("SysDebugSendIPI: not supported on this architecture");
    halt();
#endif  /* [not] CONFIG_ARCH_ARM */
}
#endif /* CONFIG_DEBUG_BUILD */

#endif /* ENABLE_SMP_SUPPORT */
