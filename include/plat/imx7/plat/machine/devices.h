/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

/* These devices are used by the seL4 kernel. */
#define UART_PADDR                  UART1_PADDR

#define UART_PPTR                   0xfff01000
#define WDT_PPTR                    0xfff02000
#define ARM_MP_PPTR1                0xfff03000
#define ARM_MP_PPTR2                0xfff04000
#define ARM_MP_PPTR3                0xfff05000
#define ARM_DEBUG_MMAPPING_PPTR     0xfff06000


#define ARM_MP_PRIV_TIMER_PPTR      (ARM_MP_PPTR1 + 0x600 )
#define ARM_MP_GLOBAL_TIMER_PPTR    (ARM_MP_PPTR1 + 0x200 )

#define GIC_PL390_CONTROLLER_PPTR   (ARM_MP_PPTR3)
#define GIC_PL390_DISTRIBUTOR_PPTR  (ARM_MP_PPTR2)


#define ARM_SCU_PADDR               0x31000000

/* All devices */
#define ARM_MP_PADDR              (ARM_SCU_PADDR) /*   2 pages */

/* addresses from Linux device tree */
#define CAAM_PADDR                0x00100000      /* 32  KiB secure RAM                     */
#define TCML_PADDR                0x007f8000      /* 32  KiB                                */
#define TCMU_PADDR                0x00800000      /* 32  KiB                                */
#define OCRAM_PADDR               0x00900000      /* 128 KiB                                */
#define OCRAM_EPDC_PADDR          0x00920000      /* 128 KiB                                */
#define OCRAM_PXP_PADDR           0x00940000      /* 32  KiB                                */
#define EIM_PADDR                 0x28000000      /* 128 MiB    NOR/SRAM                    */
/*--------------------------------- DAP memory map ---------------------------------------- */
#define DAP_ROM_PADDR             0x30000000      /* 4   KiB DAP ROM table                  */
#define CA7_DEBUG_ROM_PADDR       0x30040000      /* 4   KiB CA7 Debug ROM table            */
#define CA7_ATB_FUNNEL_PADDR      0x30041000      /* 4   KiB CA7 ATB Funnel                 */
#define CA7_ROM_PADDR             0x30060000      /* 4   KiB CA7 ROM table                  */
#define CPU0_DEBUG_PADDR          0x30070000      /* 4   KiB CPU0 debug                     */
#define CPU0_PMU_PADDR            0x30071000      /* 4   KiB CPU0 PMU                       */
#define CPU1_DEBUG_PADDR          0x30072000      /* 4   KiB CPU1 debug                     */
#define CPU1_PMU_PDDR             0x30073000      /* 4   KiB CPU1 PMU                       */
#define CPU0_CTI_PADDR            0x30078000      /* 4   KiB CPU0 CTI                       */
#define CPU1_CTI_PADDR            0x30079000      /* 4   KiB CPU1 CTI                       */
#define CPU0_ETM_PADDR            0x3007c000      /* 4   KiB CPU0 ETM                       */
#define CPU1_ETM_PADDR            0x3007d000      /* 4   KiB CPU1 ETM                       */
#define CS_HUGO_ROM_PADDR         0x30080000      /* 4   KiB CoreSight Hugo ROM table       */
#define TSGEN_CTRL_PADDR          0x30081000      /* 4   KiB                                */
#define TSGEN_READ_PADDR          0x30082000      /* 4   KiB                                */
#define ATB_FUNNEL_PADDR          0x30083000      /* 4   KiB                                */
#define TMC_ETB_PADDR             0x30084000      /* 4   KiB                                */
#define CS_HUGO_ATP_PADDR         0x30085000      /* 4   KiB CoreSight Hugo ATP replicator  */
#define TMC_ETR_PADDR             0x30086000      /* 4   KiB                                */
#define TPIU_PADDR                0x30087000      /* 4   KiB                                */
#define CS_HUGO_CTI0_PADDR        0x30088000      /* 4   KiB                                */
#define CS_HUGO_CTI1_PADDR        0x30089000      /* 4   KiB                                */

/*------------------------------- AIPS-1 memory map ----------------------------------------*/
#define AIPS1_CFG_PADDR           0x301f0000      /* 64  KiB                                */
#define GPIO1_PADDR               0x30200000      /* 64  KiB                                */
#define GPIO2_PADDR               0x30210000      /* 64  KiB                                */
#define GPIO3_PADDR               0x30220000      /* 64  KiB                                */
#define GPIO4_PADDR               0x30230000      /* 64  KiB                                */
#define GPIO5_PADDR               0x30240000      /* 64  KiB                                */
#define GPIO6_PADDR               0x30250000      /* 64  KiB                                */
#define GPIO7_PADDR               0x30260000      /* 64  KiB                                */
#define IOMUXC_LPSR_GPR_PADDR     0x30270000      /* 64  KiB                                */
#define WDOG1_PADDR               0x30280000      /* 64  KiB                                */
#define WDOG2_PADDR               0x30290000      /* 64  KiB                                */
#define WDOG3_PADDR               0x302a0000      /* 64  KiB                                */
#define WDOG4_PADDR               0x302b0000      /* 64  KiB                                */
#define IOMUXC_LPSR_PADDR         0x302c0000      /* 64  KiB                                */
#define GPT1_PADDR                0x302d0000      /* 64  KiB                                */
#define GPT2_PADDR                0x302e0000      /* 64  KiB                                */
#define GPT3_PADDR                0x302f0000      /* 64  KiB                                */
#define GPT4_PADDR                0x30300000      /* 64  KiB                                */
#define ROMCP_PADDR               0x30310000      /* 64  KiB                                */
#define KPP_PADDR                 0x30320000      /* 64  KiB                                */
#define IOMUXC_PADDR              0x30330000      /* 64  KiB                                */
#define IOMUXC_GPR_PADDR          0x30340000      /* 64  KiB                                */
#define OCOTP_CTRL_PADDR          0x30350000      /* 64  KiB                                */
#define ANALOG_PADDR              0x30360000      /* 64  KiB                                */
#define SNVS_HP_PADDR             0x30370000      /* 64  KiB                                */
#define CCM_PADDR                 0x30380000      /* 64  KiB                                */
#define SRC_PADDR                 0x30390000      /* 64  KiB                                */
#define GPC_PADDR                 0x303a0000      /* 64  KiB                                */
#define SEMAPHORE1_PADDR          0x303b0000      /* 64  KiB                                */
#define SEMAPHORE2_PADDR          0x303c0000      /* 64  KiB                                */
#define RDC_PADDR                 0x303d0000      /* 64  KiB                                */
#define CSU_PADDR                 0x303e0000      /* 64  KiB                                */
/*------------------------------- AIPS-2 mem map -------------------------------------------*/
#define AIPS2_CFG_PADDR           0x305f0000      /* 64  KiB                                */
#define ADC1_WRAPPER_PADDR        0x30610000      /* 64  KiB                                */
#define ADC2_WRAPPER_PADDR        0x30620000      /* 64  KiB                                */
#define ECSPI4_PADDR              0x30630000      /* 64  KiB                                */
#define FLEX_TIMER1_PADDR         0x30640000      /* 64  KiB                                */
#define FLEX_TIMER2_PADDR         0x30650000      /* 64  KiB                                */
#define PWM1_PADDR                0x30660000      /* 64  KiB                                */
#define PWM2_PADDR                0x30670000      /* 64  KiB                                */
#define PWM3_PADDR                0x30680000      /* 64  KiB                                */
#define PWM4_PADDR                0x30690000      /* 64  KiB                                */
#define SYS_COUNTER_RD_PADDR      0x306a0000      /* 64  KiB system counter read            */
#define SYS_COUNTER_CMP_PADDR     0x306b0000      /* 64  KiB system counter compare         */
#define SYS_COUNTER_CTRL_PADDR    0x306c0000      /* 64  KiB system counter control         */
#define PCIE_PHY_PADDR            0x306d0000      /* 64  KiB                                */
#define EDPC_PADDR                0x306f0000      /* 64  KiB                                */
#define PXP_PADDR                 0x30700000      /* 64  KiB                                */
#define CSI_PADDR                 0x30710000      /* 64  KiB                                */
#define LCDIF_PADDR               0x30730000      /* 64  KiB                                */
#define MIPI_CSI_PADDR            0x30750000      /* 64  KiB                                */
#define MIPI_DSI_PADDR            0x30760000      /* 64  KiB                                */
#define TZASC_PADDR               0x30780000      /* 64  KiB                                */
#define DDR_PHY_PADDR             0x30790000      /* 64  KiB                                */
#define DDRC_PADDR                0x307a0000      /* 64  KiB                                */
#define PERFMON1_PADDR            0x307c0000      /* 64  KiB                                */
#define PERFMON2_PADDR            0x307d0000      /* 64  KiB                                */
#define AXI_DEBUG_MON_PADDR       0x307e0000      /* 64  KiB                                */
/*------------------------------- AIPS-3 mem map -------------------------------------------*/
#define ECSPI1_PADDR              0x30820000      /* 64  KiB                                */
#define ECSPI2_PADDR              0x30830000      /* 64  KiB                                */
#define ECSPI3_PADDR              0x30840000      /* 64  KiB                                */
#define UART1_PADDR               0x30860000      /* 64  KiB                                */
#define UART2_PADDR               0x30890000      /* 64  KiB                                */
#define UART3_PADDR               0x30880000      /* 64  KiB                                */
#define SAI1_PADDR                0x308a0000      /* 64  KiB                                */
#define SAI2_PADDR                0x308b0000      /* 64  KiB                                */
#define SAI3_PADDR                0x308c0000      /* 64  KiB                                */
#define SPBA_PADDR                0x308f0000      /* 64  KiB                                */
#define AIPS3_CAAM_PADDR          0x30900000      /* 256 KiB                                */
#define AIPS3_CFG_PADDR           0x309f0000      /* 64  KiB                                */
#define FLEXCAN1_PADDR            0x30a00000      /* 64  KiB                                */
#define FLEXCAN2_PADDR            0x30a10000      /* 64  KiB                                */
#define I2C1_PADDR                0x30a20000      /* 64  KiB                                */
#define I2C2_PADDR                0x30a30000      /* 64  KiB                                */
#define I2C3_PADDR                0x30a40000      /* 64  KiB                                */
#define I2C4_PADDR                0x30a50000      /* 64  KiB                                */
#define UART4_PADDR               0x30a60000      /* 64  KiB                                */
#define UART5_PADDR               0x30a70000      /* 64  KiB                                */
#define UART6_PADDR               0x30a80000      /* 64  KiB                                */
#define UART7_PADDR               0x30a90000      /* 64  KiB                                */
#define MU_A_PADDR                0x30aa0000      /* 64  KiB                                */
#define MU_B_PADDR                0x30ab0000      /* 64  KiB                                */
#define SEMAPHORE_HS_PADDR        0x30ac0000      /* 64  KiB                                */
#define USB_PL301_PADDR           0x30ad0000      /* 64  KiB                                */
#define USB1_OTG_PADDR            0x30b10000      /* 64  KiB                                */
#define USB2_OTG_PADDR            0x30b20000      /* 64  KiB                                */
#define USB_HOST_PADDR            0x30b30000      /* 64  KiB                                */
#define USDHC1_PADDR              0x30b40000      /* 64  KiB                                */
#define USDHC2_PADDR              0x30b50000      /* 64  KiB                                */
#define USDHC3_PADDR              0x30b60000      /* 64  KiB                                */
#define SIM1_PADDR                0x30b90000      /* 64  KiB                                */
#define SIM2_PADDR                0x30ba0000      /* 64  KiB                                */
#define QSPI_PADDR                0x30bb0000      /* 64  KiB                                */
#define EIM_EIM_PADDR             0x30bc0000      /* 64  KiB                                */
#define SDMA_PADDR                0x30bd0000      /* 64  KiB                                */
#define ENET1_PADDR               0x30be0000      /* 64  KiB                                */
#define ENET2_PADDR               0x30bf0000      /* 64  KiB                                */
#define GPV0_PADDR                0x32000000      /* 1   MiB main cfg port                  */
#define GPV1_PADDR                0x32100000      /* 1   MiB wakeup cfg port                */
#define GPV2_PADDR                0x32200000      /* 1   MiB per_s cfg port                 */
#define GPV3_PADDR                0x32300000      /* 1   MiB per_m cfg port                 */
#define GPV4_PADDR                0x32400000      /* 1   MiB enet cfg port                  */
#define GPV5_PADDR                0x32500000      /* 1   MiB display cfg port               */
#define GPV6_PADDR                0x32600000      /* 1   MiB m4 cfg port                    */
#define GPV7_PADDR                0x32700000      /* 1   MiB reserved                       */
#define APBH_DMA_PADDR            0x33000000      /* 32  KiB                                */
#define QSPI1_RX_PADDR            0x34000000      /* 32  MiB  FLASH RX buffer               */
#define PCIE1_REG_PADDR           0x33800000      /* 16  KiB                                */
#define APBH_DMA_PADDR            0x33000000      /* 32  KiB                                */
#define PCIE1_PADDR               0x40000000      /* 256 MiB                                */
#define QSPI1_PADDR               0x60000000      /* 256 MiB FLASH                          */


#endif /* !__PLAT_MACHINE_DEVICES_H */
