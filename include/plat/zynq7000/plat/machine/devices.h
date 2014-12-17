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

#define L2CC_L2C310_PPTR            (L2CC_PL310_PPTR      )
#define ARM_MP_PRIV_TIMER_PPTR      (ARM_MP_PPTR1 + 0x600 )
#define ARM_MP_GLOBAL_TIMER_PPTR    (ARM_MP_PPTR1 + 0x200 )
#define GIC_PL390_CONTROLLER_PPTR   (ARM_MP_PPTR1 + 0x100 )
#define GIC_PL390_DISTRIBUTOR_PPTR  (ARM_MP_PPTR2         )


#define PL_M_AXI_GP0_PADDR 0x40000000 /* 80000000 */
#define PL_M_AXI_GP1_PADDR 0x80000000 /* C0000000 */
#define UART0_PADDR        0xE0000000 /*   1 page  */
#define UART1_PADDR        0xE0001000 /*   1 page  */
#define USB0_PADDR         0xE0002000 /*   1 page  */
#define USB1_PADDR         0xE0003000 /*   1 page  */
#define I2C0_PADDR         0xE0004000 /*   1 page  */
#define I2C1_PADDR         0xE0005000 /*   1 page  */
#define SPI0_PADDR         0xE0006000 /*   1 page  */
#define SPI1_PADDR         0xE0007000 /*   1 page  */
#define CAN0_PADDR         0xE0008000 /*   1 page  */
#define CAN1_PADDR         0xE0009000 /*   1 page  */
#define GPIO_PADDR         0xE000A000 /*   1 page  */
#define ETH0_PADDR         0xE000B000 /*   1 page  */
#define ETH1_PADDR         0xE000C000 /*   1 page  */
#define QSPI_PADDR         0xE000D000 /*   1 page  */
#define SMC_PADDR          0xE000E000 /*   1 page  */
#define SDIO0_PADDR        0xE0100000 /*   1 page  */
#define SDIO1_PADDR        0xE0101000 /*   1 page  */
#define SMC_NAND_PADDR     0xE1000000
#define SMC_NOR0_PADDR     0xE2000000
#define SMC_NOR1_PADDR     0xE4000000
#define SMC_SLCR_PADDR     0xF8000000 /*   1 page  */
#define TRPL_TIMER0_PADDR  0xF8001000 /*   1 page  */
#define TRPL_TIMER1_PADDR  0xF8002000 /*   1 page  */
#define DMAC_S_PADDR       0xF8003000 /*   1 page  */
#define DMAC_NS_PADDR      0xF8004000 /*   1 page  */
#define SWDT_PADDR         0xF8005000 /*   1 page  */
#define DDRC_PADDR         0xF8006000 /*   1 page  */
#define DEVCFG_PADDR       0xF8007000 /*   1 page  */
#define AXI_HP0_PADDR      0xF8008000 /*   1 page  */
#define AXI_HP1_PADDR      0xF8009000 /*   1 page  */
#define AXI_HP2_PADDR      0xF800A000 /*   1 page  */
#define AXI_HP3_PADDR      0xF800B000 /*   1 page  */
#define OCM_PADDR          0xF800C000 /*   1 page  */
#define EFUSE_PADDR        0xF800D000
#define CORESIGHT_PADDR    0xF8800000
#define TLICFG_GPV_PADDR   0xF8900000 /* F8A00000 */
#define MPCORE_PRIV_PADDR  0xF8F00000 /*   1 page  */
#define GIC_DIST_PADDR     0xF8F01000 /*   1 page  */
#define L2CC_PL310_PADDR   0xF8F02000 /*   1 page  */
#define QSPI_LINEAR_PADDR  0xFC000000 /* FDFF_FFFF */
#define OCM_HIGH_PADDR     0xFFFC0000 /* FFFF_FFFF */


#endif /* !__PLAT_MACHINE_DEVICES_H */

