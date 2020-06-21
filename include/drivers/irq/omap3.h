/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/kernel/vspace.h>
#include <linker.h>
#include <armv/machine.h>
#include <machine/interrupt.h>

#define INTCPS_SIR_IRQ_SPURIOUSIRQFLAG 0xFF0000

enum irqNumbers {
    irqInvalid = 255
};

/*
 * The struct below is used to discourage the compiler from generating literals
 * for every single address we might access.
 */
volatile struct INTC_map {
    uint32_t padding[4];
    uint32_t intcps_sysconfig;
    uint32_t intcps_sysstatus;
    uint32_t padding2[10];
    uint32_t intcps_sir_irq;
    uint32_t intcps_sir_fiq;
    uint32_t intcps_control;
    uint32_t intcps_protection;
    uint32_t intcps_idle;
    uint32_t padding3[3];
    uint32_t intcps_irq_priority;
    uint32_t intcps_fiq_priority;
    uint32_t intcps_threshold;
    uint32_t padding4[5];
    struct {
        uint32_t intcps_itr;
        uint32_t intcps_mir;
        uint32_t intcps_mir_clear;
        uint32_t intcps_mir_set;
        uint32_t intcps_isr_set;
        uint32_t intcps_isr_clear;
        uint32_t intcps_pending_irq;
        uint32_t intcps_pending_fiq;
    } intcps_n[3];
    uint32_t padding5[8];
    uint32_t intcps_ilr[96];
} *intc = (volatile void *)INTC_PPTR;

static inline irq_t getActiveIRQ(void)
{
    uint32_t intcps_sir_irq = intc->intcps_sir_irq;
    irq_t irq = (irq_t)(intcps_sir_irq & 0x7f);

    /* Ignore spurious interrupts. */
    if ((intcps_sir_irq & INTCPS_SIR_IRQ_SPURIOUSIRQFLAG) == 0) {
        assert(irq <= maxIRQ);
        if (intc->intcps_n[irq / 32].intcps_pending_irq & (1 << (irq & 31))) {
            return irq;
        }
    }

    /* No interrupt. */
    return irqInvalid;
}

/* Check for pending IRQ */
static inline bool_t isIRQPending(void)
{
    return getActiveIRQ() != irqInvalid;
}

/* Enable or disable irq according to the 'disable' flag. */
static inline void maskInterrupt(bool_t disable, irq_t irq)
{
    if (likely(irq < maxIRQ)) {
        if (disable) {
            intc->intcps_n[irq / 32].intcps_mir_set = 1 << (irq & 31);
        } else {
            intc->intcps_n[irq / 32].intcps_mir_clear = 1 << (irq & 31);
        }
    }
}

static inline void ackInterrupt(irq_t irq)
{
    intc->intcps_control = 1;
    /* Ensure the ack has hit the interrupt controller before potentially
     * re-enabling interrupts. */
    dsb();
}

static inline void handleSpuriousIRQ(void)
{
    /* Reset and re-enable IRQs. */
    intc->intcps_control = 1;
    dsb();
}

