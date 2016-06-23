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
#define L2CC_PL310_PPTR             0xfff02000
#define ARM_MP_PPTR1                0xfff03000
#define ARM_MP_PPTR2                0xfff04000
#define ARM_DEBUG_MMAPPING_PPTR     0xfff05000

#define L2CC_L2C310_PPTR            (L2CC_PL310_PPTR      )
#define ARM_MP_PRIV_TIMER_PPTR      (ARM_MP_PPTR1 + 0x600 )
#define ARM_MP_GLOBAL_TIMER_PPTR    (ARM_MP_PPTR1 + 0x200 )
#define GIC_PL390_CONTROLLER_PPTR   (ARM_MP_PPTR1 + 0x100 )
#define GIC_PL390_DISTRIBUTOR_PPTR  (ARM_MP_PPTR2         )


#define PL_M_AXI_GP0_PADDR         0x40000000
#define PL_M_AXI_GP1_PADDR         0x80000000
#define UART0_PADDR                0xE0000000
#define UART1_PADDR                0xE0001000
#define USB0_PADDR                 0xE0002000
#define USB1_PADDR                 0xE0003000
#define I2C0_PADDR                 0xE0004000
#define I2C1_PADDR                 0xE0005000
#define SPI0_PADDR                 0xE0006000
#define SPI1_PADDR                 0xE0007000
#define CAN0_PADDR                 0xE0008000
#define CAN1_PADDR                 0xE0009000
#define GPIO_PADDR                 0xE000A000
#define ETH0_PADDR                 0xE000B000
#define ETH1_PADDR                 0xE000C000
#define QSPI_PADDR                 0xE000D000
#define SMC_PADDR                  0xE000E000
#define SDIO0_PADDR                0xE0100000
#define SDIO1_PADDR                0xE0101000
#define SMC_NAND_PADDR             0xE1000000
#define SMC_NOR0_PADDR             0xE2000000
#define SMC_NOR1_PADDR             0xE4000000
#define SMC_SLCR_PADDR             0xF8000000
#define TRPL_TIMER0_PADDR          0xF8001000
#define TRPL_TIMER1_PADDR          0xF8002000
#define DMAC_S_PADDR               0xF8003000
#define DMAC_NS_PADDR              0xF8004000
#define SWDT_PADDR                 0xF8005000
#define DDRC_PADDR                 0xF8006000
#define DEVCFG_PADDR               0xF8007000
#define AXI_HP0_PADDR              0xF8008000
#define AXI_HP1_PADDR              0xF8009000
#define AXI_HP2_PADDR              0xF800A000
#define AXI_HP3_PADDR              0xF800B000
#define OCM_PADDR                  0xF800C000
#define EFUSE_PADDR                0xF800D000
#define DEBUG_DAP_ROM_PADDR        0xF8800000
#define DEBUG_ETB_PADDR            0xF8801000
#define DEBUG_CTI_ETB_TPIU_PADDR   0xF8802000
#define DEBUG_TPIU_PADDR           0xF8803000
#define DEBUG_FUNNEL_PADDR         0xF8804000
#define DEBUG_ITM_PADDR            0xF8805000
#define DEBUG_CTI_FTM_PADDR        0xF8809000
#define DEBUG_FTM_PADDR            0xF880B000
#define DEBUG_CPU_PMU0_PADDR       0xF8891000
#define DEBUG_CPU_PMU1_PADDR       0xF8893000
#define DEBUG_CPU_CTI0_PADDR       0xF8898000
#define DEBUG_CPU_CTI1_PADDR       0xF8899000
#define DEBUG_CPU_PTM0_PADDR       0xF889C000
#define DEBUG_CPU_PTM1_PADDR       0xF889D000
#define GPV_TRUSTZONE_PADDR        0xF8900000
#define GPV_QOS301_CPU_PADDR       0xF8946000
#define GPV_QOS301_DMAC_PADDR      0xF8947000
#define GPV_QOS301_IOU_PADDR       0xF8948000
#define MPCORE_PRIV_PADDR          0xF8F00000
#define GIC_DIST_PADDR             0xF8F01000
#define L2CC_PL310_PADDR           0xF8F02000
#define QSPI_LINEAR_PADDR          0xFC000000
#define OCM_HIGH_PADDR             0xFFFC0000


#endif /* !__PLAT_MACHINE_DEVICES_H */

