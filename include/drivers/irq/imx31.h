/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <basic_types.h>
#include <machine/interrupt.h>

enum irqNumbers {
    irqInvalid = 255
};

/* Memory map for AVIC (Advanced Vectored Interrupt Controller). */
volatile struct avic_map {
    uint32_t intctl;
    uint32_t nimask;
    uint32_t intennum;
    uint32_t intdisnum;
    uint32_t intenableh;
    uint32_t intenablel;
    uint32_t inttypeh;
    uint32_t inttypel;
    uint32_t nipriority[8];
    uint32_t nivecsr;
    uint32_t fivecsr;
    uint32_t intsrch;
    uint32_t intsrcl;
    uint32_t intfrch;
    uint32_t intfrcl;
    uint32_t nipndh;
    uint32_t nipndl;
    uint32_t fipndh;
    uint32_t fipndl;
    uint32_t vector[64];
} *avic = (volatile void *)AVIC_PPTR;

/* Reading the IRQ number from the nivecsr register also
 * acks the interrupt. To allow the active irq to be read
 * multiple times per interrupt received, we store the
 * current active IRQ in a global variable.
 */
extern irq_t active_irq;

/* Get the active IRQ number from the AVIC.  Returns 0xff if
 * there isn't one. Note this is also known as irqInvalid */
static inline irq_t getActiveIRQ(void)
{
    if (active_irq == irqInvalid) {
        /* Read the IRQ number from the IRQ controller.
         * This has the side-effect of acking the interrupt.
         * Reading from this register after acking the
         * interrupt will yield an invalid IRQ number, so
         * we save the IRQ number in a global variable to
         * allow multiple successive calls to this function.
         */
        active_irq = (avic->nivecsr >> 16) & 0xff;
    }

    return active_irq;
}

/* Check for pending IRQ */
static inline bool_t isIRQPending(void)
{
    return getActiveIRQ() != irqInvalid;
}

/* Enable or disable irq according to the 'disable' flag. */
static inline void maskInterrupt(bool_t disable, irq_t irq)
{
    if (disable) {
        avic->intdisnum = irq;
    } else {
        avic->intennum = irq;
    }
}

static inline void ackInterrupt(irq_t irq)
{
    /* The interrupt was acked when the IRQ number was read from
     * the IRQ controller in getActiveIRQ. Here we reset the
     * global active IRQ number so the next call to getActiveIRQ
     * will read the IRQ number from the IRQ controller.
     */
    active_irq = irqInvalid;
}

static inline void handleSpuriousIRQ(void)
{
    /* do nothing */
}

