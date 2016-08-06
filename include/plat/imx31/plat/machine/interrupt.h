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

/* Get the active IRQ number from the AVIC.  Returns 0xff if
 * there isn't one. Note this is also known as irqInvalid */
/**
   DONT_TRANSLATE
 */
static inline interrupt_t
getActiveIRQ(void)
{
    return (avic->nivecsr >> 16) & 0xff;
}

/* Check for pending IRQ */
static inline bool_t
isIRQPending(void)
{
    return getActiveIRQ() != irqInvalid;
}

/* Enable or disable irq according to the 'disable' flag. */
/**
   DONT_TRANSLATE
*/
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
    /* empty on this platform */
}

static inline void
handleSpuriousIRQ(void)
{
    /* do nothing */
}

#endif
