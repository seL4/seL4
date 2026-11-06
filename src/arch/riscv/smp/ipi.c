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

/*
 * Set ipiIrq to irq_remote_call_ipi so handleInterrupts calls us again
 * instead of reporting a spurious interrupt. That extra call is ignored,
 * as handleIPI() only calls us while a call is pending.
 * If it's irq_reschedule_ipi, let handleInterrupts handle it as usual.
 */
void handleRemoteCall(IpiRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2, bool_t irqPath)
{
    word_t core = getCurrentCPUIndex();

    if (ipiIrq[core] == irqInvalid) {
        ipiIrq[core] = irq_remote_call_ipi;
    }
    switch (call) {
    case IpiRemoteCall_Stall:
        /* This never returns if irqPath is false: */
        ipiStallCoreCallback(irqPath);
        break;

#ifdef CONFIG_HAVE_FPU
    case IpiRemoteCall_switchFpuOwner:
        switchLocalFpuOwner((tcb_t *)arg0);
        break;
#endif /* CONFIG_HAVE_FPU */

    default:
        fail("Invalid remote call");
        break;
    }
    big_kernel_lock.node[core].ipi = 0;
    ipi_wait();
}

void ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking)
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
}

/* this function is called with a single hart id. */
void ipi_send_target(irq_t irq, word_t hart_id)
{
    word_t hart_mask = BIT(hart_id);
    word_t core = hartIDToCoreID(hart_id);

    assert(core < CONFIG_MAX_NUM_NODES);

    /* Use clh_is_ipi_pending to check for pending call IPIs, ipiIrq
     * is only used for reschedule and to avoid spurious IRQs. */
    if (irq == irq_reschedule_ipi) {
        ipiIrq[core] = irq_reschedule_ipi;
    }
    fence_rw_rw();
    sbi_send_ipi(hart_mask);
}

#endif
