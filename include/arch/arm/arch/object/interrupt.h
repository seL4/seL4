/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>
#include <machine/interrupt.h>
#include <plat/machine.h>

exception_t Arch_decodeIRQControlInvocation(word_t invLabel, word_t length,
                                            cte_t *srcSlot, word_t *buffer);

exception_t decodeSGISignalInvocation(word_t invLabel, word_t length,
                                      cap_t cap, word_t *buffer);

/* Handle a platform-reserved IRQ. */
static inline void handleReservedIRQ(irq_t irq)
{

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    if (IRQT_TO_IRQ(irq) == KERNEL_PMU_IRQ) {
        handleOverflowIRQ();
        return;
    }
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    if (IRQT_TO_IRQ(irq) == INTERRUPT_VGIC_MAINTENANCE) {
        VGICMaintenance();
        return;
    }

    if (irqVPPIEventIndex(irq) != VPPIEventIRQ_invalid) {
        VPPIEvent(irq);
        return;
    }
#endif

#ifdef CONFIG_TK1_SMMU
    if (IRQT_TO_IRQ(irq) == INTERRUPT_SMMU) {
        plat_smmu_handle_interrupt();
        return;
    }
#endif

#ifdef CONFIG_IRQ_REPORTING
    printf("Received unhandled reserved IRQ: 0x%lx\n", IRQT_TO_IRQ(irq));
#endif
}


static inline exception_t Arch_checkIRQ(word_t irq_w)
{
    if (irq_w > maxIRQ) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = maxIRQ;
        userError("Rejecting request for IRQ %u. IRQ is greater than maxIRQ.", (int)irq_w);
        return EXCEPTION_SYSCALL_ERROR;
    }
    return EXCEPTION_NONE;
}

