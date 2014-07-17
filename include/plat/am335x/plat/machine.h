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

#define N_INTERRUPTS 128

enum IRQConstants {
    DMTIMER0_IRQ = 66,
    maxIRQ = 127
} platform_interrupt_t;

#define KERNEL_TIMER_IRQ    DMTIMER0_IRQ

enum irqNumbers {
    irqInvalid = 255
};

typedef uint8_t interrupt_t;
typedef uint8_t irq_t;

#endif
