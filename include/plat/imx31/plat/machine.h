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

#define KERNEL_TIMER_IRQ INTERRUPT_EPIT1

#define N_INTERRUPTS 64

enum IRQConstants {
    INTERRUPT_PMU = 23,
    INTERRUPT_EPIT1 = 28,
    maxIRQ = 63
} platform_interrupt_t;

enum irqNumbers {
    irqInvalid = 255
};

typedef uint32_t interrupt_t;
typedef uint32_t irq_t;

#endif
