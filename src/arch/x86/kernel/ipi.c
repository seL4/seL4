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
#include <mode/kernel/ipi.h>
#include <arch/kernel/ipi.h>
#include <arch/kernel/lock.h>
#include <model/smp.h>

#if CONFIG_MAX_NUM_NODES > 1

static volatile struct {
    word_t count;
    word_t globalsense;

    PAD_TO_NEXT_CACHE_LN(sizeof(word_t) + sizeof(word_t));
} ipiSyncBarrier = {0};                  /* IPI barrier for remote call synchronization */

static volatile word_t totalCoreBarrier; /* number of cores involved in IPI 'in progress' */
static IpiModeRemoteCall_t remoteCall;   /* the remote call being requested */
static word_t ipi_args[MAX_IPI_ARGS];    /* data to be passed to the remote call function */

static inline word_t get_ipi_arg(word_t n)
{
    assert(n < MAX_IPI_ARGS);
    return ipi_args[n];
}

static inline void ipi_wait(word_t cores)
{
    word_t localsense = ipiSyncBarrier.globalsense;

    if (__sync_fetch_and_add(&ipiSyncBarrier.count, 1) == cores) {
        ipiSyncBarrier.count = 0;
        ipiSyncBarrier.globalsense =
            ~ipiSyncBarrier.globalsense;
    }

    while (localsense == ipiSyncBarrier.globalsense) {
        asm volatile("pause");
    }
}

/* This function switches the core it is called on to the idle thread,
 * in order to avoid IPI storms. If the core is waiting on the lock, the actual
 * switch will not occur until the core attempts to obtain the lock, at which
 * point the core will capture the pending IPI, which is discarded.

 * The core who triggered the store is responsible for triggering a reschedule,
 * or this call will idle forever */
static void ipiStallCoreCallback(void)
{
    if (clh_is_self_in_queue() &&
            (NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers[Error] == -1)) {
        /* The current thread is runnable as we would replace this thread with an idle thread.
         * The instruction should be re-executed if we are in kernel to handle syscalls */
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);

        SCHED_ENQUEUE_CURRENT_TCB;
        switchToIdleThread();
        NODE_STATE(ksSchedulerAction) = SchedulerAction_ResumeCurrentThread;

        /* Let the cpu requesting this IPI to continue while we waiting on lock */
        big_kernel_lock.node_owners[getCurrentCPUIndex()].ipi = 0;
        ipi_wait(totalCoreBarrier);

        /* Continue waiting on lock */
        while (big_kernel_lock.node_owners[getCurrentCPUIndex()].next->value != CLHState_Granted) {
            if (clh_is_ipi_pending(getCurrentCPUIndex())) {

                /* Multiple calls for similar reason could result in stack overflow */
                assert((IpiRemoteCall_t)remoteCall != IpiRemoteCall_Stall);
                Arch_handleIPI(irq_remote_call_ipi);
            }
            asm volatile("pause");
        }

        /* make sure no resource access passes from this point */
        asm volatile("" ::: "memory");

        /* Start idle thread to capture the pending IPI */
        activateThread();
        restore_user_context();
    } else {
        /* We get here either without grabbing the lock from normal interrupt path or from
         * inside the lock while waiting to grab the lock for handling pending interrupt.
         * In latter case, we return to the 'clh_lock_acquire' to grab the lock and
         * handle the pending interrupt. Its valid as interrups are async events! */
        SCHED_ENQUEUE_CURRENT_TCB;
        switchToIdleThread();

        NODE_STATE(ksSchedulerAction) = SchedulerAction_ResumeCurrentThread;
    }
}

static void handleRemoteCall(IpiModeRemoteCall_t call, word_t arg0, word_t arg1)
{
    /* we gets spurious irq_remote_call_ipi calls, e.g. when handling IPI
     * in lock while hardware IPI is pending. Guard against spurious IPIs! */
    if (clh_is_ipi_pending(getCurrentCPUIndex())) {
        switch ((IpiRemoteCall_t)call) {
        case IpiRemoteCall_Stall:
            ipiStallCoreCallback();
            break;

        case IpiRemoteCall_InvalidatePageStructureCacheASID:
            invalidateLocalPageStructureCacheASID(arg0, arg1);
            break;

        case IpiRemoteCall_InvalidateTranslationSingle:
            invalidateLocalTranslationSingle(arg0);
            break;

        case IpiRemoteCall_InvalidateTranslationSingleASID:
            invalidateLocalTranslationSingleASID(arg0, arg1);
            break;

        case IpiRemoteCall_InvalidateTranslationAll:
            invalidateLocalTranslationAll();
            break;

        case IpiRemoteCall_switchFpuOwner:
            switchLocalFpuOwner((user_fpu_state_t *)arg0);
            break;

#ifdef CONFIG_VTX
        case IpiRemoteCall_ClearCurrentVCPU:
            clearCurrentVCPU();
            break;
#endif
        default:
            Mode_handleRemoteCall(call, arg0, arg1);
            break;
        }

        big_kernel_lock.node_owners[getCurrentCPUIndex()].ipi = 0;
        ipi_wait(totalCoreBarrier);
    }
}

static void handleReschedule(void)
{
    rescheduleRequired();
}

void Arch_handleIPI(irq_t irq)
{
    if (irq == irq_remote_call_ipi) {
        handleRemoteCall(remoteCall, get_ipi_arg(0), get_ipi_arg(1));
    } else if (irq == irq_reschedule_ipi) {
        handleReschedule();
    } else {
        fail("Invalid IPI");
    }
}

/* make sure all cpu IDs for number of core fit in bitwise word */
compile_assert(invalid_number_of_supported_nodes, CONFIG_MAX_NUM_NODES <= wordBits);

void doRemoteMaskOp(IpiRemoteCall_t func, word_t data1, word_t data2, word_t mask)
{
    word_t nr_target_cores = 0;
    uint16_t target_cores[CONFIG_MAX_NUM_NODES];

    /* make sure the current core is not set in the mask */
    mask &= ~BIT(getCurrentCPUIndex());

    /* this may happen, e.g. the caller tries to map a pagetable in
     * newly created PD which has not been run yet. Guard against them! */
    if (mask != 0) {

        /* setup the data and choose the requested cpu to send IPI*/
        remoteCall = func;
        ipi_args[0] = data1;
        ipi_args[1] = data2;
        while (mask) {
            int index = wordBits - 1 - clzl(mask);
            target_cores[nr_target_cores] = index;
            nr_target_cores++;
            mask &= ~BIT(index);
        }

        /* sending IPIs... */
        totalCoreBarrier = nr_target_cores;
        asm volatile("" ::: "memory");

        for (int i = 0; i < nr_target_cores; i++) {
            big_kernel_lock.node_owners[target_cores[i]].ipi = 1;
            apic_send_ipi(int_remote_call_ipi, cpuIndexToID(target_cores[i]));
        }

        ipi_wait(totalCoreBarrier);
    }
}

void doMaskReschedule(word_t mask)
{
    /* make sure the current core is not set in the mask */
    mask &= ~BIT(getCurrentCPUIndex());

    while (mask) {
        int index = wordBits - 1 - clzl(mask);
        apic_send_ipi(int_reschedule_ipi, cpuIndexToID(index));
        mask &= ~BIT(index);
    }
}

#endif /* CONFIG_MAX_NUM_NODES */
