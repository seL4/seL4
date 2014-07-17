/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_H
#define __PLAT_MACHINE_H

#include <arch/machine/gic_pl390.h>

#define N_INTERRUPTS             (maxIRQ + 1)

enum IRQConstants {
// INTERRUPT_RESERVED          =  0,
// INTERRUPT_RESERVED          =  1,
// INTERRUPT_RESERVED          =  2,
// INTERRUPT_RESERVED          =  3,
// INTERRUPT_RESERVED          =  4,
// INTERRUPT_RESERVED          =  5,
// INTERRUPT_RESERVED          =  6,
// INTERRUPT_RESERVED          =  7,
// INTERRUPT_RESERVED          =  8,
// INTERRUPT_RESERVED          =  9,
// INTERRUPT_RESERVED          = 10,
// INTERRUPT_RESERVED          = 11,
// INTERRUPT_RESERVED          = 12,
// INTERRUPT_RESERVED          = 13,
// INTERRUPT_RESERVED          = 14,
// INTERRUPT_RESERVED          = 15,

    INTERRUPT_KPSS_DBGT         = 17,
    INTERRUPT_KPSS_GPT0         = 18,
    INTERRUPT_KPSS_GPT1         = 19,
    INTERRUPT_KPSS_WDT0         = 20,
    INTERRUPT_KPSS_WDT1         = 21,

    INTERRUPT_RPM_GPT0          = 66,
    INTERRUPT_RPM_GPT1          = 66,
    INTERRUPT_RPM_WDOG          = 68,

    INTERRUPT_PPSS_XOTMR0        = 241, /* ? */
    INTERRUPT_PPSS_XOTMR1        = 242, /* ? */
    INTERRUPT_PPSS_SLPTMR0       = 243, /* ? */
    INTERRUPT_PPSS_SLPTMR1       = 244, /* ? */
    INTERRUPT_PPSS_WDOG          = 245,

#if 0
    maxIRQ                      = 283
#else /* SELFOUR-222 */
    maxIRQ                      = 245
#endif
} platform_interrupt_t;

#define KERNEL_TIMER_IRQ INTERRUPT_KPSS_DBGT

enum irqNumbers {
    irqInvalid = (irq_t) - 1
};

#endif /* !__PLAT_MACHINE_H */
