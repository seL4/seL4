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

#ifndef __PLAT_MACHINE_H
#define __PLAT_MACHINE_H

enum IRQConstants {
    INTERRUPT_SGI_0                 = 0,
    INTERRUPT_SGI_1                 = 1,
    INTERRUPT_SGI_2                 = 2,
    INTERRUPT_SGI_3                 = 3,
    INTERRUPT_SGI_4                 = 4,
    INTERRUPT_SGI_5                 = 5,
    INTERRUPT_SGI_6                 = 6,
    INTERRUPT_SGI_7                 = 7,
    INTERRUPT_SGI_8                 = 8,
    INTERRUPT_SGI_9                 = 9,
    INTERRUPT_SGI_10                = 10,
    INTERRUPT_SGI_11                = 11,
    INTERRUPT_SGI_12                = 12,
    INTERRUPT_SGI_13                = 13,
    INTERRUPT_SGI_14                = 14,
    INTERRUPT_SGI_15                = 15,
    INTERRUPT_PPI_0                 = 16,
    INTERRUPT_PPI_1                 = 17,
    INTERRUPT_PPI_2                 = 18,
    INTERRUPT_PPI_3                 = 19,
    INTERRUPT_PPI_4                 = 20,
    INTERRUPT_PPI_5                 = 21,
    INTERRUPT_PPI_6                 = 22,
    INTERRUPT_PPI_7                 = 23,
    INTERRUPT_PPI_8                 = 24,
    INTERRUPT_PPI_9                 = 25,
    INTERRUPT_PPI_10                = 26,
    INTERRUPT_PPI_11                = 27,
    INTERRUPT_PPI_12                = 28,
    INTERRUPT_PPI_13                = 29,
    INTERRUPT_PPI_14                = 30,
    INTERRUPT_PPI_15                = 31,
    LIC_START_INTERRUPT             = 32,
    INTERRUPT_SMMU0                 = 202,
    /*
     * HOST1X_SYNCPT_CPU is the interrupt for the first core PMU.
     * Obtained from the dts
     */
    HOST1X_SYNCPT_CPU               = 264,
    LIC_END_INTERRUPT               = LIC_START_INTERRUPT + 288 - 1,
    LOCAL_SPI_START_INTERRUPT       = LIC_START_INTERRUPT + 288,
    LOCAL_SPI_END_INTERRUPT         = LOCAL_SPI_START_INTERRUPT + 64 - 1,
    maxIRQ                          = LOCAL_SPI_END_INTERRUPT
} platform_interrupt_t;

compile_assert(interuppt_mapping_check, (maxIRQ == (32 + 288 + 64 - 1)))

#define IRQ_CNODE_BITS      14

#define INTERRUPT_VGIC_MAINTENANCE  INTERRUPT_PPI_9
#define INTERRUPT_HGPT              INTERRUPT_PPI_10
#define INTERRUPT_VGPT              INTERRUPT_PPI_11

#define KERNEL_TIMER_IRQ    INTERRUPT_VGPT

#define KERNEL_PMU_IRQ      HOST1X_SYNCPT_CPU

#include <arch/machine/gic_pl390.h>

#endif  /* ! __PLAT_MACHINE_H */
