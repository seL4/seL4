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
static IpiRemoteCall_t remoteCall;       /* the remote call being requested */
static word_t remoteCallData;            /* data to be passed to the remote call function */

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

static void handleRemoteCall(void)
{
    /* we gets spurious irq_remote_call_ipi calls, e.g. when handling IPI
     * in lock while hardware IPI is pending. Guard against spurious IPIs! */
    if (clh_is_ipi_pending(getCurrentCPUIndex())) {
        switch (remoteCall) {
        case IpiRemoteCall_Null:

            break;
        default:
            fail("Invalid remote call");
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
        handleRemoteCall();
    } else if (irq == irq_reschedule_ipi) {
        handleReschedule();
    } else {
        fail("Invalid IPI");
    }
}

/* make sure all cpu IDs for number of core fit in bitwise word */
compile_assert(invalid_number_of_supported_nodes, CONFIG_MAX_NUM_NODES <= wordBits);

void doRemoteMaskOp(IpiRemoteCall_t func, word_t data, word_t mask)
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
        remoteCallData = data;
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

void doReschedule(word_t cpu)
{
    if (cpu != getCurrentCPUIndex() &&
            cpu < CONFIG_MAX_NUM_NODES) {
        apic_send_ipi(int_remote_call_ipi, cpuIndexToID(cpu));
    }
}

#endif
