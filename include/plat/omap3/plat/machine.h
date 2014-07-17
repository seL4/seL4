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

#define N_INTERRUPTS 96

enum IRQConstants {
    EMUINT          =  0,
    COMMTX          =  1,
    COMMRX          =  2,
    BENCH           =  3,
    MCBSP2_ST_IRQ   =  4,
    MCBSP3_ST_IRQ   =  5,
    RESERVED_IRQ01  =  6,
    SYS_NIRQ        =  7,
    RESERVED_IRQ02  =  8,
    SMX_DBG_IRQ     =  9,
    SMX_APP_IRQ     = 10,
    PRCM_MPU_IRQ    = 11,
    SDMA_IRQ_0      = 12,
    SDMA_IRQ_1      = 13,
    SDMA_IRQ_2      = 14,
    SDMA_IRQ_3      = 15,
    MCBSP1_IRQ      = 16,
    MCBSP2_IRQ      = 17,
    SR1_IRQ         = 18,
    SR2_IRQ         = 19,
    GPMC_IRQ        = 20,
    SGX_IRQ         = 21,
    MCBSP3_IRQ      = 22,
    MCBSP4_IRQ      = 23,
    CAM_IRQ0        = 24,
    DSS_IRQ         = 25,
    MAIL_U0_MPU_IRQ = 26,
    MCBSP5_IRQ      = 27,
    IVA2_MMU_IRQ    = 28,
    GPIO1_MPU_IRQ   = 29,
    GPIO2_MPU_IRQ   = 30,
    GPIO3_MPU_IRQ   = 31,
    GPIO4_MPU_IRQ   = 32,
    GPIO5_MPU_IRQ   = 33,
    GPIO6_MPU_IRQ   = 34,
    RESERVED_IRQ03  = 35,
    WDT3_IRQ        = 36,
    GPT1_IRQ        = 37,
    GPT2_IRQ        = 38,
    GPT3_IRQ        = 39,
    GPT4_IRQ        = 40,
    GPT5_IRQ        = 41,
    GPT6_IRQ        = 42,
    GPT7_IRQ        = 43,
    GPT8_IRQ        = 44,
    GPT9_IRQ        = 45,
    GPT10_IRQ       = 46,
    GPT11_IRQ       = 47,
    SPI4_IRQ        = 48,
    RESERVED_IRQ04  = 49,
    RESERVED_IRQ05  = 50,
    RESERVED_IRQ06  = 51,
    RESERVED_IRQ07  = 52,
    RESERVED_IRQ08  = 53,
    MCBSP4_IRQ_TX   = 54,
    MCBSP4_IRQ_RX   = 55,
    I2C1_IRQ        = 56,
    I2C2_IRQ        = 57,
    HDQ_IRQ         = 58,
    MCBSP1_IRQ_TX   = 59,
    MCBSP1_IRQ_RX   = 60,
    I2C3_IRQ        = 61,
    MCBSP2_IRQ_TX   = 62,
    MCBSP2_IRQ_RX   = 63,
    RESERVED_IRQ09  = 64,
    SPI1_IRQ        = 65,
    SPI2_IRQ        = 66,
    RESERVED_IRQ10  = 67,
    RESERVED_IRQ11  = 68,
    RESERVED_IRQ12  = 69,
    RESERVED_IRQ13  = 70,
    RESERVED_IRQ14  = 71,
    UART1_IRQ       = 72,
    UART2_IRQ       = 73,
    UART3_IRQ       = 74,
    PBIAS_IRQ       = 75,
    OHCI_IRQ        = 76,
    EHCI_IRQ        = 77,
    TLL_IRQ         = 78,
    RESERVED_IRQ15  = 79,
    UART4_IRQ       = 80,
    MCBSP5_IRQ_TX   = 81,
    MCBSP5_IRQ_RX   = 82,
    MMC1_IRQ        = 83,
    RESERVED_IRQ16  = 84,
    RESERVED_IRQ17  = 85,
    MMC2_IRQ        = 86,
    MPU_ICR_IRQ     = 87,
    D2DFRINT        = 88,
    MCBSP3_IRQ_TX   = 89,
    MCBSP3_IRQ_RX   = 90,
    SPI3_IRQ        = 91,
    HSUSB_MC_NINT   = 92,
    HSUSB_DMA_NINT  = 93,
    MMC3_IRQ        = 94,
    RESERVED_IRQ18  = 95,

    //GPTIMER11_IRQ = 47,
    maxIRQ = 95
} platform_interrupt_t;



#define KERNEL_TIMER_IRQ    GPT11_IRQ

enum irqNumbers {
    irqInvalid = 255
};

typedef uint8_t interrupt_t;
typedef uint8_t irq_t;

#endif  /* ! __PLAT_MACHINE_H */
