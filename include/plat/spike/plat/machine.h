/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 * Copyright (c) 2018, Hesham Almatary <Hesham.Almatary@cl.cam.ac.uk>
 * All rights reserved.
 *
 * This software was was developed in part by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __PLAT_MACHINE_H
#define __PLAT_MACHINE_H

#include <config.h>

#define N_INTERRUPTS 7

#ifndef __ASSEMBLER__
enum IRQConstants {
    INTERRUPT_SW = 0,
    INTERRUPT_STIMER = 5,
    INTERRUPT_MTIMER = 7,
    /* TODO: Handle PLIC and add external IRQs upon needed */
    maxIRQ = 7
} platform_interrupt_t;

#define IRQ_CNODE_BITS 12

#ifdef CONFIG_SEL4_RV_MACHINE
#define KERNEL_TIMER_IRQ INTERRUPT_MTIMER
#else
#define KERNEL_TIMER_IRQ INTERRUPT_STIMER
#endif

enum irqNumbers {
    irqInvalid = 8
};

typedef uint32_t interrupt_t;
typedef uint32_t irq_t;

#endif /* !__ASSEMBLER__ */
#endif
