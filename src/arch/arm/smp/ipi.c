/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <mode/smp/ipi.h>
#include <smp/lock.h>
#include <util.h>

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
        word_t totalCoreBarrier = args->totalCoreBarrier;

        switch ((IpiRemoteCall_t)call) {
        case IpiRemoteCall_Stall:
            ipiStallCoreCallback(irqPath);
            break;

#ifdef CONFIG_HAVE_FPU
        case IpiRemoteCall_switchFpuOwner:
            switchLocalFpuOwner((user_fpu_state_t *)arg0);
            break;
#endif /* CONFIG_HAVE_FPU */

        case IpiRemoteCall_InvalidateTranslationSingle:
            invalidateTranslationSingleLocal(arg0);
            break;

        case IpiRemoteCall_InvalidateTranslationASID:
            invalidateTranslationASIDLocal(arg0);
            break;

        case IpiRemoteCall_InvalidateTranslationAll:
            invalidateTranslationAllLocal();
            break;

        case IpiRemoteCall_MaskPrivateInterrupt:
            maskInterrupt(arg0, IDX_TO_IRQT(arg1));
            break;

#if defined CONFIG_ARM_HYPERVISOR_SUPPORT && defined ENABLE_SMP_SUPPORT
        case IpiRemoteCall_VCPUInjectInterrupt: {
            virq_t virq;
            virq.words[0] = args->args[2];
            handleVCPUInjectInterruptIPI((vcpu_t *) arg0, arg1, virq);
            break;
        }
#endif

        default:
            fail("Invalid remote call");
            break;
        }

        big_kernel_lock.node_owners[core].ipi = NULL;
        ipi_wait(totalCoreBarrier);
    }
}

void ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking)
{
    generic_ipi_send_mask(ipi, mask, isBlocking);
}
#endif /* ENABLE_SMP_SUPPORT */
