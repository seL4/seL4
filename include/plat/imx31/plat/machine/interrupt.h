/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_INTERRUPT_H
#define __PLAT_MACHINE_INTERRUPT_H

#include <config.h>
#include <basic_types.h>
#include <arch/benchmark_overflowHandler.h>
#include <plat/machine.h>

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
extern interrupt_t active_irq;

/* Get the active IRQ number from the AVIC.  Returns 0xff if
 * there isn't one. Note this is also known as irqInvalid */
static inline interrupt_t
getActiveIRQ(void)
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
static inline bool_t
isIRQPending(void)
{
    return getActiveIRQ() != irqInvalid;
}

/* Enable or disable irq according to the 'disable' flag. */
static inline void
maskInterrupt(bool_t disable, interrupt_t irq)
{
    if (disable) {
        avic->intdisnum = irq;
    } else {
        avic->intennum = irq;
    }
}

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    if (irq == KERNEL_PMU_IRQ) {
        handleOverflowIRQ();
    }
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
}

static inline void
ackInterrupt(irq_t irq)
{
    /* The interrupt was acked when the IRQ number was read from
     * the IRQ controller in getActiveIRQ. Here we reset the
     * global active IRQ number so the next call to getActiveIRQ
     * will read the IRQ number from the IRQ controller.
     */
    active_irq = irqInvalid;
}

static inline void
handleSpuriousIRQ(void)
{
    /* do nothing */
}

#endif
