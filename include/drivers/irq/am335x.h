/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <machine/interrupt.h>
#include <armv/machine.h>

enum irqNumbers {
    irqInvalid = 255
};

#define CMPER_REG(base, off) ((volatile uint32_t *)((base) + (off)))
#define CMPER_TIMER3_CLKCTRL    0x84
#define CMPER_TIMER4_CLKCTRL    0x88

#define CMPER_CLKCTRL_DISABLE   0
#define CMPER_CLKCTRL_ENABLE    2

#define CMPER_CLKSEL_TIMER3     0x50c
#define CMPER_CLKSEL_TIMER4     0x510
#define CMPER_CKLSEL_MOSC       1

#define RESERVED                3

#define INTCPS_SYSCONFIG_SOFTRESET BIT(1)
#define INTCPS_SYSSTATUS_RESETDONE BIT(0)
#define INTCPS_CONTROL_NEWIRQAGR BIT(0)
#define INTCPS_SIR_IRQ_SPURIOUSIRQFLAG 0xffffff80

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
    } intcps_n[4];
    uint32_t intcps_ilr[128];
} *intc = (volatile void *)INTC_PPTR;


static inline irq_t getActiveIRQ(void)
{
    uint32_t intcps_sir_irq = intc->intcps_sir_irq;
    irq_t irq = (irq_t)(intcps_sir_irq & 0x7f);

    if ((intcps_sir_irq & INTCPS_SIR_IRQ_SPURIOUSIRQFLAG) == 0) {
        assert((irq / 32) < (sizeof intc->intcps_n / sizeof intc->intcps_n[0]));
        if (intc->intcps_n[irq / 32].intcps_pending_irq & (1 << (irq & 31))) {
            return irq;
        }
    }
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
    /*
     * am335x ref man, sec 6.2.2 only requires a DSB after NEWIRQAGR.
     * I found that without dsb() or more code before, I get interrupts
     * without the associated pending bit being set. Perhaps this
     * indicates a missing barrier in code elsewhere? -TimN
     */
    dsb();
    intc->intcps_control = INTCPS_CONTROL_NEWIRQAGR;
    dsb();
}

static inline void handleSpuriousIRQ(void)
{
    /* Reset and re-enable IRQs. */
    intc->intcps_control = INTCPS_CONTROL_NEWIRQAGR;
    dsb();
}

