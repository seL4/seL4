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

static volatile irq_t ipiIrq[CONFIG_MAX_NUM_NODES];

void arch_handleRemoteCall(IpiRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2)
{
    switch (call) {
    case IpiRemoteCall_Stall:
        ipiStallCoreCallback();
        break;
#ifdef CONFIG_HAVE_FPU
    case IpiRemoteCall_switchFpuOwner:
        switchLocalFpuOwner(NULL);
        break;
#endif /* CONFIG_HAVE_FPU */
    default:
        fail("Invalid remote call");
        break;
    }
}

void arch_ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking)
{
    generic_ipi_send_mask(ipi, mask, isBlocking);
}

irq_t ipi_get_irq(void)
{
    word_t core = getCurrentCPUIndex();

    assert(!clh_is_ipi_pending(core));
    return ipiIrq[core];
}

void ipi_clear_irq(irq_t irq)
{
    word_t core = getCurrentCPUIndex();

    ipiIrq[core] = irqInvalid;
    /* This should probably be a dedicated IPI instead of a side-effect. */
    ifence_local();
}

/* this function is called with a single hart id. */
void arch_ipi_send_target(irq_t irq, word_t hart_id)
{
    word_t hart_mask = BIT(hart_id);
    word_t core = hartIDToCoreID(hart_id);

    assert(core < CONFIG_MAX_NUM_NODES);
    /*
     * clh_is_ipi_pending() is used to check for pending call IPIs,
     * ipiIrq is only used for reschedule and to avoid spurious IRQs.
     * Don't overwrite reschedule IPIs, otherwise they will be lost.
     */
    if (ipiIrq[core] != irq_reschedule_ipi) {
        ipiIrq[core] = irq;
    }
    fence_rw_rw();
    sbi_send_ipi(hart_mask);
}

#endif
