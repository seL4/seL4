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

#define N_INTERRUPTS     160

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
    INTERRUPT_GLOBAL_TIMER  =  27,
    INTERRUPT_PL_FIQ        =  28,
    INTERRUPT_PRIV_TIMER    =  29,
    INTERRUPT_PRIV_WDT      =  30,
    INTERRUPT_PL_F2P        =  31,
    INTERRUPT_APU_CPU0      =  32,
    INTERRUPT_APU_CPU1      =  33,
    INTERRUPT_APU_L2CC      =  34,
    INTERRUPT_APU_OCM       =  35,
//  INTERRUPT_RESERVED      =  36,
    INTERRUPT_PMU0          =  37,
    INTERRUPT_PMU1          =  38,
    INTERRUPT_XADC          =  39,
    INTERRUPT_DEVC          =  40,
    INTERRUPT_SWDT          =  41,
    INTERRUPT_TTIMER0_0     =  42,
    INTERRUPT_TTIMER0_1     =  43,
    INTERRUPT_TTIMER0_2     =  44,
    INTERRUPT_DMAC_ABORT    =  45,
    INTERRUPT_DMAC0         =  46,
    INTERRUPT_DMAC1         =  47,
    INTERRUPT_DMAC2         =  48,
    INTERRUPT_DMAC3         =  49,
    INTERRUPT_MEM_SMC       =  50,
    INTERRUPT_MEM_QSPI      =  51,
    INTERRUPT_GPIO          =  52,
    INTERRUPT_USB0          =  53,
    INTERRUPT_ETH0          =  54,
    INTERRUPT_ETH0_WAKEUP   =  55,
    INTERRUPT_SDIO0         =  56,
    INTERRUPT_I2C0          =  57,
    INTERRUPT_SPI0          =  58,
    INTERRUPT_UART0         =  59,
    INTERRUPT_CAN0          =  60,
    INTERRUPT_PL00          =  61,
    INTERRUPT_PL01          =  62,
    INTERRUPT_PL02          =  63,
    INTERRUPT_PL03          =  64,
    INTERRUPT_PL04          =  65,
    INTERRUPT_PL05          =  66,
    INTERRUPT_PL06          =  67,
    INTERRUPT_PL07          =  68,
    INTERRUPT_TTIMER1_0     =  69,
    INTERRUPT_TTIMER1_1     =  70,
    INTERRUPT_TTIMER1_2     =  71,
    INTERRUPT_DMAC4         =  72,
    INTERRUPT_DMAC5         =  73,
    INTERRUPT_DMAC6         =  74,
    INTERRUPT_DMAC7         =  75,
    INTERRUPT_USB1          =  76,
    INTERRUPT_ETH1          =  77,
    INTERRUPT_ETH1_WAKEUP   =  78,
    INTERRUPT_SDIO1         =  79,
    INTERRUPT_I2C1          =  80,
    INTERRUPT_SPI1          =  81,
    INTERRUPT_UART1         =  82,
    INTERRUPT_CAN1          =  83,
    INTERRUPT_PL08          =  84,
    INTERRUPT_PL09          =  85,
    INTERRUPT_PL10          =  86,
    INTERRUPT_PL11          =  87,
    INTERRUPT_PL12          =  88,
    INTERRUPT_PL13          =  89,
    INTERRUPT_PL14          =  90,
    INTERRUPT_PL15          =  91,
    INTERRUPT_SCU_PARITY    =  92,
    maxIRQ = 92
} platform_interrupt_t;

#define IRQ_CNODE_BITS 12

#define KERNEL_TIMER_IRQ INTERRUPT_PRIV_TIMER
#define KERNEL_PMU_IRQ INTERRUPT_PMU0

#include <arch/machine/gic_pl390.h>

#endif /* !__PLAT_MACHINE_H */
