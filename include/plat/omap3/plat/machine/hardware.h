/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#include <basic_types.h>
#include <linker.h>
#include <armv/machine.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <plat/machine/interrupt.h>

#define physBase          0x80000000
#define kernelBase        0xf0000000

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  GP Timer 9 */
        GPTIMER9_PADDR,
        GPTIMER9_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  INTC */
        INTC_PADDR,
        INTC_PPTR,
        true  /* armExecuteNever */
#ifdef CONFIG_PRINTING
    },
    {
        /*  UART */
        UART3_PADDR,
        UART3_PPTR,
        true  /* armExecuteNever */
#endif
    }
};

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 512 MiB */
    { .start = 0x80000000, .end = 0xA0000000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    /* sorted by increasing memory address */
    /* region caps must be a power of 2. */


    /* Boot space */
    /* 0x00000000 - 0x40000000 */
//  { GPMC_PADDR                    , GPMC_PADDR                     + ( 1 << 30       ) },

    /* TODO: Board specific devices should ultimately be replaced with a more general solution. */
    { ETHERNET_BASE_PADDR           , ETHERNET_BASE_PADDR            + ( 1 << PAGE_BITS) },

    /* On-chip memory */
    /* 0x40000000 - 0x48000000 */
//  { BOOT_ROM0_PADDR               , BOOT_ROM0_PADDR                + (20 << PAGE_BITS) },
//  { BOOT_ROM1_PADDR               , BOOT_ROM1_PADDR                + ( 8 << PAGE_BITS) },
//  { SRAM_INTERNAL_PADDR           , SRAM_INTERNAL_PADDR            + (16 << PAGE_BITS) },

    /* L4 core (2 pages each unless specified) */
    /* 0x48000000 - 0x48300000 */
    { SYSTEM_CONTROL_MODULE_PADDR   , SYSTEM_CONTROL_MODULE_PADDR    + ( 2 << PAGE_BITS) },
    { CLOCK_MANAGER_PADDR           , CLOCK_MANAGER_PADDR            + ( 2 << PAGE_BITS) },
    { L4_CORE_CONFIG_PADDR          , L4_CORE_CONFIG_PADDR           + ( 2 << PAGE_BITS) },
    { DISPLAY_SUBSYSTEM_PADDR       , DISPLAY_SUBSYSTEM_PADDR        + ( 2 << PAGE_BITS) },
    { SDMA_PADDR                    , SDMA_PADDR                     + ( 2 << PAGE_BITS) },
    { I2C3_PADDR                    , I2C3_PADDR                     + ( 2 << PAGE_BITS) },
    { USBTLL_PADDR                  , USBTLL_PADDR                   + ( 2 << PAGE_BITS) },
    { HS_USB_HOST_PADDR             , HS_USB_HOST_PADDR              + ( 2 << PAGE_BITS) },
    { UART1_PADDR                   , UART1_PADDR                    + ( 2 << PAGE_BITS) },
    { UART2_PADDR                   , UART2_PADDR                    + ( 2 << PAGE_BITS) },
    { I2C1_PADDR                    , I2C1_PADDR                     + ( 2 << PAGE_BITS) },
    { I2C2_PADDR                    , I2C2_PADDR                     + ( 2 << PAGE_BITS) },
    { MCBSP1_PADDR                  , MCBSP1_PADDR                   + ( 2 << PAGE_BITS) },
    { GPTIMER10_PADDR               , GPTIMER10_PADDR                + ( 2 << PAGE_BITS) },
    { GPTIMER11_PADDR               , GPTIMER11_PADDR                + ( 2 << PAGE_BITS) },
    { MAILBOX_PADDR                 , MAILBOX_PADDR                  + ( 2 << PAGE_BITS) },
    { MCBSP5_PADDR                  , MCBSP5_PADDR                   + ( 2 << PAGE_BITS) },
    { MCSPI1_PADDR                  , MCSPI1_PADDR                   + ( 2 << PAGE_BITS) },
    { MCSPI2_PADDR                  , MCSPI2_PADDR                   + ( 2 << PAGE_BITS) },
    { MMC_SD_SDIO1_PADDR            , MMC_SD_SDIO1_PADDR             + ( 2 << PAGE_BITS) },
    { HS_USB_OTG_PADDR              , HS_USB_OTG_PADDR               + ( 2 << PAGE_BITS) },
    { MMC_SD_SDIO3_PADDR            , MMC_SD_SDIO3_PADDR             + ( 2 << PAGE_BITS) },
    { HDQ_TM_1WIRE_PADDR            , HDQ_TM_1WIRE_PADDR             + ( 2 << PAGE_BITS) },
    { MMC_SD_SDIO2_PADDR            , MMC_SD_SDIO2_PADDR             + ( 2 << PAGE_BITS) },
    { ICR_MPU_PORT_PADDR            , ICR_MPU_PORT_PADDR             + ( 2 << PAGE_BITS) },
    { MCSPI3_PADDR                  , MCSPI3_PADDR                   + ( 2 << PAGE_BITS) },
    { MCSPI4_PADDR                  , MCSPI4_PADDR                   + ( 2 << PAGE_BITS) },
    { CAMERA_ISP_PADDR              , CAMERA_ISP_PADDR               + ( 2 << PAGE_BITS) },
    { SR1_PADDR                     , SR1_PADDR                      + ( 2 << PAGE_BITS) },
    { SR2_PADDR                     , SR2_PADDR                      + ( 2 << PAGE_BITS) },
    { ICR_MODEM_PORT_PADDR          , ICR_MODEM_PORT_PADDR           + ( 2 << PAGE_BITS) },
//  { INTC_PADDR                    , INTC_PADDR                     + ( 1 << PAGE_BITS) },
    { L4_WAKEUP_INTERCONNECT_A_PADDR, L4_WAKEUP_INTERCONNECT_A_PADDR + ( 2 << PAGE_BITS) },
    { CONTROL_MODULE_ID_CODE_PADDR  , CONTROL_MODULE_ID_CODE_PADDR   + ( 2 << PAGE_BITS) },
    { L4_WAKEUP_INTERCONNECT_B_PADDR, L4_WAKEUP_INTERCONNECT_B_PADDR + ( 2 << PAGE_BITS) },

    /* L4 Wakeup (2 pages each unless specified) */
    /* 0x48300000 - 0x49000000 */
    { PWR_AND_RST_MANAGER_PADDR     , PWR_AND_RST_MANAGER_PADDR      + ( 2 << PAGE_BITS) },
    { GPIO1_PADDR                   , GPIO1_PADDR                    + ( 2 << PAGE_BITS) },
    { WDT2_PADDR                    , WDT2_PADDR                     + ( 2 << PAGE_BITS) },
    { GPTIMER1_PADDR                , GPTIMER1_PADDR                 + ( 2 << PAGE_BITS) },
    { TIMER32K_PADDR                , TIMER32K_PADDR                 + ( 2 << PAGE_BITS) },
    { L4_WAKEUP_CONFIG_PADDR        , L4_WAKEUP_CONFIG_PADDR         + ( 2 << PAGE_BITS) },

    /* L4 peripherals (2 pages each) */
    /* 0x49000000 - 0x50000000 */
    { L4_PER_CONFIG_PADDR           , L4_PER_CONFIG_PADDR            + ( 2 << PAGE_BITS) },
    { UART3_PADDR                   , UART3_PADDR                    + ( 2 << PAGE_BITS) },
    { MCBSP2_PADDR                  , MCBSP2_PADDR                   + ( 2 << PAGE_BITS) },
    { MCBSP3_PADDR                  , MCBSP3_PADDR                   + ( 2 << PAGE_BITS) },
    { MCBSP4_PADDR                  , MCBSP4_PADDR                   + ( 2 << PAGE_BITS) },
    { MCBSP2_SIDETONE_PADDR         , MCBSP2_SIDETONE_PADDR          + ( 2 << PAGE_BITS) },
    { MCBSP3_SIDETONE_PADDR         , MCBSP3_SIDETONE_PADDR          + ( 2 << PAGE_BITS) },
    { WDT3_PADDR                    , WDT3_PADDR                     + ( 2 << PAGE_BITS) },
    { GPTIMER2_PADDR                , GPTIMER2_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER3_PADDR                , GPTIMER3_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER4_PADDR                , GPTIMER4_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER5_PADDR                , GPTIMER5_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER6_PADDR                , GPTIMER6_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER7_PADDR                , GPTIMER7_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER8_PADDR                , GPTIMER8_PADDR                 + ( 2 << PAGE_BITS) },
//  { GPTIMER9_PADDR                , GPTIMER9_PADDR                 + ( 2 << PAGE_BITS) },
    { UART4_PADDR                   , UART4_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO2_PADDR                   , GPIO2_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO3_PADDR                   , GPIO3_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO4_PADDR                   , GPIO4_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO5_PADDR                   , GPIO5_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO6_PADDR                   , GPIO6_PADDR                    + ( 2 << PAGE_BITS) },

    /* SGX */
    /* 0x50000000 - 0x54000000 */
    { SGX_PADDR                     , SGX_PADDR                      + (16 << PAGE_BITS) },

    /* L4 emu (2 pages each unless specified) */
    /* 0x54000000 - 0x58000000 */
    { EMU_TPIU_PADDR                , EMU_TPIU_PADDR                 + ( 2 << PAGE_BITS) },
    { EMU_ETB_PADDR                 , EMU_ETB_PADDR                  + ( 2 << PAGE_BITS) },
    { EMU_DAPCTL_PADDR              , EMU_DAPCTL_PADDR               + ( 2 << PAGE_BITS) },
    { EMU_SDTI_L4_INTERCONNECT_PADDR, EMU_SDTI_L4_INTERCONNECT_PADDR + ( 1 << PAGE_BITS) },
    { EMU_SDTI_CONFIG_PADDR         , EMU_SDTI_CONFIG_PADDR          + ( 1 << PAGE_BITS) },
    { EMU_SDTI_WINDOW_PADDR         , EMU_SDTI_WINDOW_PADDR          + ( 1 << 20       ) },
    { EMU_PWR_AND_RST_MANAGER_PADDR , EMU_PWR_AND_RST_MANAGER_PADDR  + ( 4 << PAGE_BITS) },
    { EMU_GPIO1_PADDR               , EMU_GPIO1_PADDR                + ( 2 << PAGE_BITS) },
//  { EMU_WDT2_PADDR                , EMU_WDT2_PADDR                 + ( 2 << PAGE_BITS) },
    { EMU_GPTIMER1_PADDR            , EMU_GPTIMER1_PADDR             + ( 2 << PAGE_BITS) },
    { EMU_32KTIMER_PADDR            , EMU_32KTIMER_PADDR             + ( 2 << PAGE_BITS) },
    { EMU_L4_WAKEUP_CONFIG_PADDR    , EMU_L4_WAKEUP_CONFIG_PADDR     + ( 3 << PAGE_BITS) },

    /* IVA 2.2 Subsystem */
    /* 0x5C000000 - 0x60000000 */
    { IVA_22_PADDR                  , IVA_22_PADDR                   + (48 << 20       ) },

    /* Level 3 Interconnect */
    /* 0x68000000 - 0x70000000 */
    { L3_CONTROL_PADDR              , L3_CONTROL_PADDR               + (16 << 20       ) },
//  { L3_SMS_CONFIG                 , L3_SMS_CONFIG                  + (16 << 20       ) },
//  { L3_SDRC_CONFIG                , L3_SDRC_CONFIG                 + (16 << 20       ) },
//  { L3_GPMC_CONFIG                , L3_GPMC_CONFIG                 + (16 << 20       ) }
};

#endif
