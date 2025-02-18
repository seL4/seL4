/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <mode/smp/ipi.h>
#include <smp/ipi.h>
#include <smp/lock.h>

#ifdef ENABLE_SMP_SUPPORT

static void handleRemoteCall(bool_t irqPath)
{
    cpu_id_t core = getCurrentCPUIndex();
    /* we gets spurious irq_remote_call_ipi calls, e.g. when handling IPI
     * in lock while hardware IPI is pending. Guard against spurious IPIs! */
    if (clh_is_ipi_pending(core)) {
        struct ipi_args *args = (struct ipi_args *)big_kernel_lock.node_owners[core].ipi;
        IpiRemoteCall_t call = args->remoteCall;
        word_t arg0 = args->args[0];
        word_t arg1 = args->args[1];
        word_t arg2 = args->args[2];
        word_t totalCoreBarrier = args->totalCoreBarrier;

        switch ((IpiRemoteCall_t)call) {
        case IpiRemoteCall_Stall:
            ipiStallCoreCallback(irqPath);
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
        case IpiRemoteCall_VMCheckBoundNotification:
            VMCheckBoundNotification((tcb_t *)arg0);
            break;
#endif
        default:
            Mode_handleRemoteCall((IpiModeRemoteCall_t)args->remoteCall, arg0, arg1, arg2);
            break;
        }

        big_kernel_lock.node_owners[core].ipi = NULL;
        ipi_wait(totalCoreBarrier);
    }
}

/* make sure all cpu IDs for number of core fit in bitwise word */
compile_assert(invalid_number_of_supported_nodes, CONFIG_MAX_NUM_NODES <= wordBits);

#ifdef CONFIG_USE_LOGICAL_IDS
static void x86_ipi_send_mask(interrupt_t ipi, word_t mask, bool_t isBlocking)
{
    word_t nr_target_clusters = 0;
    word_t target_clusters[CONFIG_MAX_NUM_NODES];

    do {
        int core = wordBits - 1 - clzl(mask);
        target_clusters[nr_target_clusters] = 0;

        /* get mask of all cores in bitmask which are in same cluster as 'core' */
        word_t sub_mask = mask & cpu_mapping.other_indexes_in_cluster[core];
        target_clusters[nr_target_clusters] |= cpu_mapping.index_to_logical_id[core];
        if (isBlocking) {
            big_kernel_lock.node_owners[core].ipi = 1;
        }

        /* check if there is any other core in this cluster */
        while (sub_mask) {
            int index = wordBits - 1 - clzl(sub_mask);
            target_clusters[nr_target_clusters] |= cpu_mapping.index_to_logical_id[index];
            if (isBlocking) {
                big_kernel_lock.node_owners[index].ipi = 1;
            }
            sub_mask &= ~BIT(index);
        }

        mask &= ~(cpu_mapping.other_indexes_in_cluster[core] | BIT(core));
        nr_target_clusters++;
    } while (mask != 0);

    /* broadcast IPIs to clusters... */
    IPI_ICR_BARRIER;
    for (int i = 0; i < nr_target_clusters; i++) {
        apic_send_ipi_cluster(ipi, target_clusters[i]);
    }
}
#endif /* CONFIG_USE_LOGICAL_IDS */

void ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking)
{
    interrupt_t interrupt_ipi = ipi + IRQ_INT_OFFSET;

#ifdef CONFIG_USE_LOGICAL_IDS
    x86_ipi_send_mask(interrupt_ipi, mask, isBlocking);
#else
    generic_ipi_send_mask(interrupt_ipi, mask, isBlocking);
#endif /* CONFIG_USE_LOGICAL_IDS */
}
#endif /* ENABLE_SMP_SUPPORT */
