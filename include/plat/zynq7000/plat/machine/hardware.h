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

#include <config.h>
#include <basic_types.h>
#include <linker.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <arch/benchmark_overflowHandler.h>

#define physBase          0x00000000
#define kernelBase        0xe0000000

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  GIC controller and private timers */
        MPCORE_PRIV_PADDR,
        ARM_MP_PPTR1,
        true  /* armExecuteNever */
    },
    {
        /*  GIC distributor */
        MPCORE_PRIV_PADDR + BIT(PAGE_BITS),
        ARM_MP_PPTR2,
        true  /* armExecuteNever */
    },
    {
        /*  L2CC */
        L2CC_PL310_PADDR,
        L2CC_PL310_PPTR,
        true  /* armExecuteNever */
#ifdef CONFIG_PRINTING
    },
    {
        /*  UART */
        UART_PADDR,
        UART_PPTR,
        true  /* armExecuteNever */
#endif /* CONFIG_PRINTING */
    }
};

/* Available physical memory regions on platform (RAM) */
/* NOTE: Regions are not allowed to be adjacent! */
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 1 GiB */
    { /* .start = */ 0x00000000, /* .end = */ 0x40000000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    { /* .start = */ UART0_PADDR             , /* .end = */ UART0_PADDR              + ( 1 << 12)},
    { /* .start = */ UART1_PADDR             , /* .end = */ UART1_PADDR              + ( 1 << 12)},
    { /* .start = */ USB0_PADDR              , /* .end = */ USB0_PADDR               + ( 1 << 12)},
    { /* .start = */ USB1_PADDR              , /* .end = */ USB1_PADDR               + ( 1 << 12)},
    { /* .start = */ I2C0_PADDR              , /* .end = */ I2C0_PADDR               + ( 1 << 12)},
    { /* .start = */ I2C1_PADDR              , /* .end = */ I2C1_PADDR               + ( 1 << 12)},
    { /* .start = */ SPI0_PADDR              , /* .end = */ SPI0_PADDR               + ( 1 << 12)},
    { /* .start = */ SPI1_PADDR              , /* .end = */ SPI1_PADDR               + ( 1 << 12)},
    { /* .start = */ CAN0_PADDR              , /* .end = */ CAN0_PADDR               + ( 1 << 12)},
    { /* .start = */ CAN1_PADDR              , /* .end = */ CAN1_PADDR               + ( 1 << 12)},
    { /* .start = */ GPIO_PADDR              , /* .end = */ GPIO_PADDR               + ( 1 << 12)},
    { /* .start = */ ETH0_PADDR              , /* .end = */ ETH0_PADDR               + ( 1 << 12)},
    { /* .start = */ ETH1_PADDR              , /* .end = */ ETH1_PADDR               + ( 1 << 12)},
    { /* .start = */ QSPI_PADDR              , /* .end = */ QSPI_PADDR               + ( 1 << 12)},
    { /* .start = */ SMC_PADDR               , /* .end = */ SMC_PADDR                + ( 1 << 12)},
    { /* .start = */ SDIO0_PADDR             , /* .end = */ SDIO0_PADDR              + ( 1 << 12)},
    { /* .start = */ SDIO1_PADDR             , /* .end = */ SDIO1_PADDR              + ( 1 << 12)},
    { /* .start = */ SMC_NAND_PADDR          , /* .end = */ SMC_NAND_PADDR           + (16 << 20)},
    { /* .start = */ SMC_NOR0_PADDR          , /* .end = */ SMC_NOR0_PADDR           + (32 << 20)},
    { /* .start = */ SMC_NOR1_PADDR          , /* .end = */ SMC_NOR1_PADDR           + (32 << 20)},
    { /* .start = */ SMC_SLCR_PADDR          , /* .end = */ SMC_SLCR_PADDR           + ( 1 << 12)},
    { /* .start = */ TRPL_TIMER0_PADDR       , /* .end = */ TRPL_TIMER0_PADDR        + ( 1 << 12)},
    { /* .start = */ TRPL_TIMER1_PADDR       , /* .end = */ TRPL_TIMER1_PADDR        + ( 1 << 12)},
    { /* .start = */ DMAC_S_PADDR            , /* .end = */ DMAC_S_PADDR             + ( 1 << 12)},
    { /* .start = */ DMAC_NS_PADDR           , /* .end = */ DMAC_NS_PADDR            + ( 1 << 12)},
    { /* .start = */ SWDT_PADDR              , /* .end = */ SWDT_PADDR               + ( 1 << 12)},
    { /* .start = */ DDRC_PADDR              , /* .end = */ DDRC_PADDR               + ( 1 << 12)},
    { /* .start = */ DEVCFG_PADDR            , /* .end = */ DEVCFG_PADDR             + ( 1 << 12)},
    { /* .start = */ AXI_HP0_PADDR           , /* .end = */ AXI_HP0_PADDR            + ( 1 << 12)},
    { /* .start = */ AXI_HP1_PADDR           , /* .end = */ AXI_HP1_PADDR            + ( 1 << 12)},
    { /* .start = */ AXI_HP2_PADDR           , /* .end = */ AXI_HP2_PADDR            + ( 1 << 12)},
    { /* .start = */ AXI_HP3_PADDR           , /* .end = */ AXI_HP3_PADDR            + ( 1 << 12)},
    { /* .start = */ OCM_PADDR               , /* .end = */ OCM_PADDR                + ( 1 << 12)},
    { /* .start = */ EFUSE_PADDR             , /* .end = */ EFUSE_PADDR              + ( 2 << 12)},
    { /* .start = */ DEBUG_DAP_ROM_PADDR     , /* .end = */ DEBUG_DAP_ROM_PADDR      + ( 1 << 12)},
    { /* .start = */ DEBUG_ETB_PADDR         , /* .end = */ DEBUG_ETB_PADDR          + ( 1 << 12)},
    { /* .start = */ DEBUG_CTI_ETB_TPIU_PADDR, /* .end = */ DEBUG_CTI_ETB_TPIU_PADDR + ( 1 << 12)},
    { /* .start = */ DEBUG_TPIU_PADDR        , /* .end = */ DEBUG_TPIU_PADDR         + ( 1 << 12)},
    { /* .start = */ DEBUG_FUNNEL_PADDR      , /* .end = */ DEBUG_FUNNEL_PADDR       + ( 1 << 12)},
    { /* .start = */ DEBUG_ITM_PADDR         , /* .end = */ DEBUG_ITM_PADDR          + ( 1 << 12)},
    { /* .start = */ DEBUG_CTI_FTM_PADDR     , /* .end = */ DEBUG_CTI_FTM_PADDR      + ( 1 << 12)},
    { /* .start = */ DEBUG_FTM_PADDR         , /* .end = */ DEBUG_FTM_PADDR          + ( 1 << 12)},
    { /* .start = */ DEBUG_CPU_PMU0_PADDR    , /* .end = */ DEBUG_CPU_PMU0_PADDR     + ( 1 << 12)},
    { /* .start = */ DEBUG_CPU_PMU1_PADDR    , /* .end = */ DEBUG_CPU_PMU1_PADDR     + ( 1 << 12)},
    { /* .start = */ DEBUG_CPU_CTI0_PADDR    , /* .end = */ DEBUG_CPU_CTI0_PADDR     + ( 1 << 12)},
    { /* .start = */ DEBUG_CPU_CTI1_PADDR    , /* .end = */ DEBUG_CPU_CTI1_PADDR     + ( 1 << 12)},
    { /* .start = */ DEBUG_CPU_PTM0_PADDR    , /* .end = */ DEBUG_CPU_PTM0_PADDR     + ( 1 << 12)},
    { /* .start = */ DEBUG_CPU_PTM1_PADDR    , /* .end = */ DEBUG_CPU_PTM1_PADDR     + ( 1 << 12)},
    { /* .start = */ GPV_TRUSTZONE_PADDR     , /* .end = */ GPV_TRUSTZONE_PADDR      + ( 1 << 12)},
    { /* .start = */ GPV_QOS301_CPU_PADDR    , /* .end = */ GPV_QOS301_CPU_PADDR     + ( 1 << 12)},
    { /* .start = */ GPV_QOS301_DMAC_PADDR   , /* .end = */ GPV_QOS301_DMAC_PADDR    + ( 1 << 12)},
    { /* .start = */ GPV_QOS301_IOU_PADDR    , /* .end = */ GPV_QOS301_IOU_PADDR     + ( 1 << 12)},
//  { /* .start = */ MPCORE_PRIV_PADDR , /* .end = */ MPCORE_PRIV_PADDR              + ( 1 << 12)},
//  { /* .start = */ GIC_DIST_PADDR    , /* .end = */ GIC_DIST_PADDR                 + ( 1 << 12)},
//  { /* .start = */ L2CC_PL310_PADDR  , /* .end = */ L2CC_PL310_PADDR               + ( 1 << 12)},
    { /* .start = */ QSPI_LINEAR_PADDR , /* .end = */ QSPI_LINEAR_PADDR              + (32 << 20)},
    { /* .start = */ OCM_HIGH_PADDR    , /* .end = */ OCM_HIGH_PADDR                 + (64 << 12)},
    /* Programmable logic */
    { /* .start = */ PL_M_AXI_GP0_PADDR, /* .end = */ PL_M_AXI_GP0_PADDR             + 0x40000000U},
    { /* .start = */ PL_M_AXI_GP1_PADDR, /* .end = */ PL_M_AXI_GP1_PADDR             + 0x40000000U},
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    if (irq == KERNEL_PMU_IRQ) {
        handleOverflowIRQ();
    }
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
}

#endif /* !__PLAT_MACHINE_HARDWARE_H */
