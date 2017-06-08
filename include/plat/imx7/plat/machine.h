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
//  INTERRUPT_RESERVED          = 0,
//  INTERRUPT_RESERVED          = 1,
//  INTERRUPT_RESERVED          = 2,
//  INTERRUPT_RESERVED          = 3,
//  INTERRUPT_RESERVED          = 4,
//  INTERRUPT_RESERVED          = 5,
//  INTERRUPT_RESERVED          = 6,
//  INTERRUPT_RESERVED          = 7,
//  INTERRUPT_RESERVED          = 8,
//  INTERRUPT_RESERVED          = 9,
//  INTERRUPT_RESERVED          = 10,
//  INTERRUPT_RESERVED          = 11,
//  INTERRUPT_RESERVED          = 12,
//  INTERRUPT_RESERVED          = 13,
//  INTERRUPT_RESERVED          = 14,
//  INTERRUPT_RESERVED          = 15,
//  INTERRUPT_RESERVED          = 16,
//  INTERRUPT_RESERVED          = 17,
//  INTERRUPT_RESERVED          = 18,
//  INTERRUPT_RESERVED          = 19,
//  INTERRUPT_RESERVED          = 20,
//  INTERRUPT_RESERVED          = 21,
//  INTERRUPT_RESERVED          = 22,
//  INTERRUPT_RESERVED          = 23,
//  INTERRUPT_RESERVED          = 24,
//  INTERRUPT_RESERVED          = 25,
//  INTERRUPT_RESERVED          = 26,
    INTERRUPT_PPI_11            = 27,
//  INTERRUPT_RESERVED          = 28,
    INTERRUPT_PRIV_TIMER        = 29,
//  INTERRUPT_RESERVED          = 30,
//  INTERRUPT_RESERVED          = 31,
    INTERRUPT_GPR_IRQ           = 32,
    INTERRUPT_DAP               = 33,
    INTERRUPT_SDMA              = 34,      /* AND of all 48 SDMA Interrupts from all channels */
    INTERRUPT_DBGMON            = 35,
    INTERRUPT_SNVS              = 36,
    INTERRUPT_LCDIF             = 37,
    INTRRRUPT_SIM2              = 38,
    INTERRUPT_CSI               = 39,
    INTERRUPT_PXP1              = 40,
//  INTERRUPT_RESERVED          = 41,
    INTERRUPT_WDOG3             = 42,      /* watchdog timer reset                             */
    INTERRUPT_HS1               = 43,      /* hardware semaphore                               */
    INTERRUPT_APBH_DMA          = 44,      /* GPMI operation channel 0 description complete    */
    INTERRUPT_EIM               = 45,
    INTERRUPT_BCH               = 46,
    INTERRUPT_GPMI              = 47,      /* GPMI operation timeout error                     */
    INTERRUPT_UART6             = 48,      /* UART6 ORed interrupt                             */
    INTERRUPT_FTM1              = 49,      /* Flex timer 1 fault/counter/channel               */
    INTERRUPT_FTM2              = 50,      /* Flex timer 2 fault/counter/channel               */
    INTERRUPT_SNVS_HP_NS        = 51,      /* SRTC consolidated, non TZ                        */
    INTERRUPT_SNVS_HP_S         = 52,      /* SRTC security interrupt, TZ                      */
    INTRRRUPT_CSU               = 53,
    INTERRUPT_USDHC1            = 54,
    INTERRUPT_USDHC2            = 55,
    INTERRUPT_USDHC3            = 56,
    INTERRUPT_MIPI_CSI          = 57,
    INTERRUPT_UART1             = 58,
    INTERRUPT_UART2             = 59,
    INTERRUPT_UART3             = 60,
    INTERRUPT_UART4             = 61,
    INTERRUPT_UART5             = 62,
    INTERRUPT_ECSPI1            = 63,
    INTERRUPT_ECSPI2            = 64,
    INTERRUPT_ECSPI3            = 65,
    INTERRUPT_ECSPI4            = 66,
    INTERRUPT_I2C1              = 67,
    INTRRRUPT_I2C2              = 68,
    INTERRUPT_I2C3              = 69,
    INTERRUPT_I2C4              = 70,
    INTERRUPT_RDC               = 71,
    INTERRUPT_USB_HOST          = 72,
    INTERRUPT_MIPI_DSI          = 73,
    INTERRUPT_USB2_OTG_CORE     = 74,
    INTERRUPT_USB1_OTG_CORE     = 75,
    INTERRUPT_USB1_OTG_WAKEUP   = 76,
    INTERRUPT_USB2_OTG_WAKEUP   = 77,
    INTERRUPT_PXP2              = 78,
    INTERRUPT_SCTR1             = 79,
    INTERRUPT_SCTR2             = 80,
    INTERRUPT_ANALOG_TS         = 81,       /* TempSensor (temp low alarm )                 */
    INTERRUPT_SAI3              = 82,
    INTERRUPT_ANALOG_BO         = 83,       /* Brown-out event on either analog regulators  */
    INTERRUPT_GPT4              = 84,
    INTERRUPT_GPT3              = 85,
    INTERRUPT_GPT2              = 86,
    INTERRUPT_GPT1              = 87,
    INTERRUPT_GPIO1_INT7        = 88,       /* active high interrupt from INT7 from GPIO    */
    INTERRUPT_GPIO1_INT6        = 89,
    INTERRUPT_GPIO1_INT5        = 90,
    INTERRUPT_GPIO1_INT4        = 91,
    INTERRUPT_GPIO1_INT3        = 92,
    INTERRUPT_GPIO1_INT2        = 93,
    INTERRUPT_GPIO1_INT1        = 94,
    INTERRUPT_GPIO1_INT0        = 95,
    INTERRUPT_GPIO1_0_15        = 96,       /* combined interrupt for GPIO1 0 - 15          */
    INTERRUPT_GPIO1_16_31       = 97,
    INTERRUPT_GPIO2_0_15        = 98,
    INTERRUPT_GPIO2_16_31       = 99,
    INTERRUPT_GPIO3_0_15        = 100,
    INTERRUPT_GPIO3_16_31       = 101,
    INTERRUPT_GPIO4_0_15        = 102,
    INTERRUPT_GPIO4_16_31       = 103,
    INTERRUPT_GPIO5_0_15        = 104,
    INTERRUPT_GPIO5_16_31       = 105,
    INTERRUPT_GPIO6_0_15        = 106,
    INTERRUPT_GPIO6_16_31       = 107,
    INTERRUPT_GPIO7_0_15        = 108,
    INTERRUPT_GPIO7_16_31       = 109,
    INTERRUPT_WDOG1             = 110,
    INTERRUPT_WDOG2             = 111,
    INTERRUPT_KPP               = 112,      /* Keypad interrupt                             */
    INTERRUPT_PWM1              = 113,
    INTERRUPT_PWM2              = 114,
    INTRRRUPT_PWM3              = 115,
    INTERRUPT_PWM4              = 116,
    INTERRUPT_CCM1              = 117,
    INTERRUPT_CCM2              = 118,
    INTERRUPT_GPC               = 119,
    INTERRUPT_MU_A7             = 120,      /* Interrupt to A7                              */
    INTERRUPT_SRC               = 121,
    INTERRUPT_SIM1              = 122,
    INTERRUPT_RTIC              = 123,
    INTERRUPT_CPU_PMU           = 124,
    INTERRUPT_CPU_CTI           = 125,
    INTERRUPT_CCM_SRC_GPC       = 126,      /* combined CPU wdog interrupts 4x out of SRC   */
    INTERRUPT_SAI1              = 127,
    INTERRUPT_SAI2              = 128,
    INTERRUPT_MU_M4             = 129,      /* interrupt to M4                              */
    INTERRUPT_ADC1              = 130,
    INTERRRUPT_ADC2             = 131,
    INTERRUPT_ENET2_RXTX1       = 132,      /* MAC 0 rx/tx frame / buffer done              */
    INTERRUPT_ENET2_RXTX2       = 133,      /* MAC 0 rx/tx frame / buffer done              */
    INTERRUPT_ENET2_MAC0        = 134,      /* MAC 0 IRQ                                    */
    INTERRUPT_ENET2_MAC0_TIMER  = 135,      /* MAC 0 1588 timer                             */
    INTERRUPT_TPR               = 136,
    INTERRUPT_CAAM_1            = 137,      /* CAAM interrupt queue for JQ                  */
    INTERRUPT_CAAM_2            = 138,
    INTERRUPT_QSPI              = 139,
    INTERRUPT_TZASC1            = 140,      /* TZASC (PL380)                                 */
    INTERRUPT_WDOG4             = 141,
    INTERRUPT_FLEXCAN1          = 142,
    INTERRUPT_FLEXCAN2          = 143,
    INTERRUPT_PERFMON1          = 144,
    INTERRUPT_PERFMON2          = 145,
    INTERRUPT_CAAM_3            = 146,
    INTERRUPT_CAAM_4            = 147,      /* recoverable error interrupt                  */
    INTERRUPT_HS2               = 148,
    INTERRUPT_EPDC              = 149,
    INTERRUPT_ENET1_RXTX1       = 150,
    INTERRUPT_ENET1_RXTX2       = 151,
    INTERRUPT_ENET1_MAC0        = 152,
    INTERRUPT_ENET1_MAC0_TIMER  = 153,
    INTERRUPT_PCIE_CTRL1        = 154,      /* PCIE INT A or MSI if enabled                 */
    INTERRUPT_PCIE_CTRL2        = 155,      /* PCIE INT B                                   */
    INTERRUPT_PCIE_CTRL3        = 156,      /* PCIE INT C                                   */
    INTERRUPT_PCIE_CTRL4        = 157,      /* PCIE INT D                                   */
    INTERRUPT_UART7             = 158,
    INTERRUPT_PCIE_CTRL5        = 159,      /* PCIE channels[63:32]                         */
    maxIRQ                      = 159
} platform_interrupt_t;

#define IRQ_CNODE_BITS 12

#define KERNEL_TIMER_IRQ  INTERRUPT_PPI_11

#endif /* !__PLAT_MACHINE_H */
