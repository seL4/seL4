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

#define N_INTERRUPTS     160

/* pull some device interrupts from Linux device tree, need to
 * confirm them when the offcial manual is available.
 */
enum IRQConstants {
//  INTERRUPT_RESERVED      =   0,
//  INTERRUPT_RESERVED      =   1,
//  INTERRUPT_RESERVED      =   2,
//  INTERRUPT_RESERVED      =   3,
//  INTERRUPT_RESERVED      =   4,
//  INTERRUPT_RESERVED      =   5,
//  INTERRUPT_RESERVED      =   6,
//  INTERRUPT_RESERVED      =   7,
//  INTERRUPT_RESERVED      =   8,
//  INTERRUPT_RESERVED      =   9,
//  INTERRUPT_RESERVED      =  10,
//  INTERRUPT_RESERVED      =  11,
//  INTERRUPT_RESERVED      =  12,
//  INTERRUPT_RESERVED      =  13,
//  INTERRUPT_RESERVED      =  14,
//  INTERRUPT_RESERVED      =  15,
//  INTERRUPT_RESERVED      =  16,
//  INTERRUPT_RESERVED      =  17,
//  INTERRUPT_RESERVED      =  18,
//  INTERRUPT_RESERVED      =  19,
//  INTERRUPT_RESERVED      =  20,
//  INTERRUPT_RESERVED      =  21,
//  INTERRUPT_RESERVED      =  22,
//  INTERRUPT_RESERVED      =  23,
//  INTERRUPT_RESERVED      =  24,
//  INTERRUPT_RESERVED      =  25,
//  INTERRUPT_RESERVED      =  26,
//  INTERRUPT_RESERVED      =  27,
//  INTERRUPT_RESERVED      =  28,
    INTERRUPT_PRIV_TIMER    =  29,
//  INTERRUPT_RESERVED      =  30,
//  INTERRUPT_RESERVED      =  31,
    INTERRUPT_IOMUXC        =  32,
    INTERRUPT_GPIO0_15_0    =  96,
    INTERRUPT_GPIO0_31_16   =  97,
    INTERRUPT_GPIO1_15_0    =  98,
    INTERRUPT_GPIO1_31_16   =  99,
    INTERRUPT_GPIO2_15_0    =  100,
    INTERRUPT_GPIO2_31_16   =  101,
    INTERRUPT_GPIO3_15_0    =  102,
    INTERRUPT_GPIO3_31_16   =  103,
    INTERRUPT_GPIO4_15_0    =  104,
    INTERRUPT_GPIO4_31_16   =  105,
    INTERRUPT_GPIO5_15_0    =  106,
    INTERRUPT_GPIO5_31_16   =  107,
    INTERRUPT_GPIO6_15_0    =  108,
    INTERRUPT_GPIO6_31_16   =  109,
    INTERRUPT_WDOG0         =  110,
    INTERRUPT_WDOG1         =  111,
    INTERRUPT_WDOG2         =  42,
    INTERRUPT_WDOG3         =  141,
    INTERRUPT_GPT0          =  87,
    INTERRUPT_GPT1          =  86,
    INTERRUPT_GPT2          =  85,
    INTERRUPT_GPT3          =  84,
    INTERRUPT_ANATOP_0      =  81,
    INTERRUPT_ANATOP_1      =  83,

    maxIRQ = 159
} platform_interrupt_t;

#define IRQ_CNODE_BITS 12

#define KERNEL_TIMER_IRQ  INTERRUPT_PRIV_TIMER

enum irqNumbers {
    irqInvalid = (irq_t) - 1
};

#endif /* !__PLAT_MACHINE_H */
