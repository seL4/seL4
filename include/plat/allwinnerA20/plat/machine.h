/*
 * Copyright 2015, DornerWorks, Ltd.
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

#define N_INTERRUPTS 122

enum IRQConstants {
    SGI_0           = 0,
    SGI_1           = 1,
    SGI_2           = 2,
    SGI_3           = 3,
    SGI_4           = 4,
    SGI_5           = 5,
    SGI_6           = 6,
    SGI_7           = 7,
    SGI_8           = 8,
    SGI_9           = 9,
    SGI_10          = 10,
    SGI_11          = 11,
    SGI_12          = 12,
    SGI_13          = 13,
    SGI_14          = 14,
    SGI_15          = 15,
    PPI_0           = 16,
    PPI_1           = 17,
    PPI_2           = 18,
    PPI_3           = 19,
    PPI_4           = 20,
    PPI_5           = 21,
    PPI_6           = 22,
    PPI_7           = 23,
    PPI_8           = 24,
    PPI_9           = 25,
    PPI_10          = 26,
    PPI_11          = 27,
    PPI_12          = 28,
    PPI_13          = 29,
    PPI_14          = 30,
    PPI_15          = 31,
    NMI             = 32,
    UART_0          = 33,
    UART_1          = 34,
    UART_2          = 35,
    UART_3          = 36,
    IR_0            = 37,
    IR_1            = 38,
    TWI_0           = 39,
    TWI_1           = 40,
    TWI_2           = 41,
    SPI_0           = 42,
    SPI_1           = 43,
    SPI_2           = 44,
    OWA             = 45,
    AC97            = 46,
    TS              = 47,
    IIS0            = 48,
    UART_4          = 49,
    UART_5          = 50,
    UART_6          = 51,
    UART_7          = 52,
    KEYPAD          = 53,
    TIMER_0         = 54,
    TIMER_1         = 55,
    TIMER_2         = 56,
    TIMER_3         = 57,
    CAN             = 58,
    DMA             = 59,
    PIO             = 60,
    TOUCH_PANEL     = 61,
    AUDIO_CODEC     = 62,
    LRADC           = 63,
    MMC_0           = 64,
    MMC_1           = 65,
    MMC_2           = 66,
    MMC_3           = 67,
    MS              = 68,
    NAND            = 69,
    USB_0           = 70,
    USB_1           = 71,
    USB_2           = 72,
    SCR             = 73,
    CSI_0           = 74,
    CSI_1           = 75,
    LCD_0           = 76,
    LCD_1           = 77,
    MP              = 78,
    DE_FE0          = 79,
    DE_FE1          = 80,
    PMU             = 81,
    SPI3            = 82,
    RESERVED_IRQ01  = 83,
    RESERVED_IRQ02  = 84,
    VE              = 85,
    SS              = 86,
    EMAC            = 87,
    SATA            = 88,
    RESERVED_IRQ03  = 89,
    HDMI_0          = 90,
    TVE             = 91,
    ACE             = 92,
    TVD             = 93,
    PS2_0           = 94,
    PS2_1           = 95,
    USB_3           = 96,
    USB_4           = 97,
    PLE             = 98,
    TIMER_4         = 99,
    TIMER_5         = 100,
    GPU_GP          = 101,
    GPU_GPMMU       = 102,
    GPU_PP0         = 103,
    GPU_PPMMU0      = 104,
    GPU_PMU         = 105,
    GPU_PP1         = 106,
    GPU_PPMMU1      = 107,
    GPU_RSV0        = 108,
    GPU_RSV1        = 109,
    GPU_RSV2        = 110,
    GPU_RSV3        = 111,
    GPU_RSV4        = 112,
    HS_TIMER_0      = 113,
    HS_TIMER_1      = 114,
    HS_TIMER_2      = 115,
    HS_TIMER_3      = 116,
    GMAC            = 117,
    HDMI_1          = 118,
    IIS1            = 119,
    TWI_3           = 120,
    TWI_4           = 121,
    IIS_2           = 122,
    maxIRQ = 122
} platform_interrupt_t;

#define IRQ_CNODE_BITS 12

#define KERNEL_TIMER_IRQ    TIMER_0

enum irqNumbers {
    irqInvalid = 255
};

#endif  /* ! __PLAT_MACHINE_H */
