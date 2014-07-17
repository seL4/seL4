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
#define UART_PADDR                  UART2_PADDR

#define UART_PPTR                   0xfff01000
#define WDT_PPTR                    0xfff02000
#define L2CC_PL310_PPTR             0xfff03000
#define ARM_MP_PPTR1                0xfff04000
#define ARM_MP_PPTR2                0xfff05000

#define L2CC_L2C310_PPTR            (L2CC_PL310_PPTR      )
#define ARM_MP_PRIV_TIMER_PPTR      (ARM_MP_PPTR1 + 0x600 )
#define ARM_MP_GLOBAL_TIMER_PPTR    (ARM_MP_PPTR1 + 0x200 )
#define GIC_PL390_CONTROLLER_PPTR   (ARM_MP_PPTR1 + 0x100 )
#define GIC_PL390_DISTRIBUTOR_PPTR  (ARM_MP_PPTR2         )


/* All devices */
#define EIM_CS0_PADDR             0x08000000 /* 128 MB    */
//#define RESERVED_PADDR          0x02C00000 /*  84 MB    */
#define IPU2_PADDR                0x02800000 /*   4 MB    */
#define IPU1_PADDR                0x02400000 /*   4 MB    */
//#define RESERVED_PADDR          0x0220C000 /*   1 MB    */
#define MIPI_HSI_PADDR            0x02208000 /*   4 pages */
#define OPENVG_PADDR              0x02204000 /*   4 pages */
#define SATA_PADDR                0x02200000 /*   4 pages */
//#define RESERVED_PADDR          0x021FC000 /*   4 pages */
//#define RESERVED_PADDR          0x021F8000 /*   4 pages */
#define UART5_PADDR               0x021F4000 /*   4 pages */
#define UART4_PADDR               0x021F0000 /*   4 pages */
#define UART3_PADDR               0x021EC000 /*   4 pages */
#define UART2_PADDR               0x021E8000 /*   4 pages */
#define VDOA_PADDR                0x021E4000 /*   4 pages */
#define MIPI_DSI_PADDR            0x021E0000 /*   4 pages */
#define MIPI_CSI_PADDR            0x021DC000 /*   4 pages */
#define AUDMUX_PADDR              0x021D8000 /*   4 pages */
#define TZASC2_PADDR              0x021D4000 /*   4 pages */
#define TZASC1_PADDR              0x021D0000 /*   4 pages */
//#define RESERVED_PADDR          0x021CC000 /*   4 pages */
//#define RESERVED_PADDR          0x021C8000 /*   4 pages */
//#define RESERVED_PADDR          0x021C4000 /*   4 pages */
#define CSU_PADDR                 0x021C0000 /*   4 pages */
#define OCOTP_CTRL_PADDR          0x021BC000 /*   4 pages */
#define EIM_PADDR                 0x021B8000 /*   4 pages */
#define MMDC1_PADDR               0x021B4000 /*   4 pages */
#define MMDC0_PADDR               0x021B0000 /*   4 pages */
#define ROMCP_PADDR               0x021AC000 /*   4 pages */
#define I2C3_PADDR                0x021A8000 /*   4 pages */
#define I2C2_PADDR                0x021A4000 /*   4 pages */
#define I2C1_PADDR                0x021A0000 /*   4 pages */
#define USDHC4_PADDR              0x0219C000 /*   4 pages */
#define USDHC3_PADDR              0x02198000 /*   4 pages */
#define USDHC2_PADDR              0x02194000 /*   4 pages */
#define USDHC1_PADDR              0x02190000 /*   4 pages */
#define MLB150_PADDR              0x0218C000 /*   4 pages */
#define ENET_PADDR                0x02188000 /*   4 pages */
#define USBOH3_PADDR              0x02184000 /*   4 pages */
//#define RESERVED_PADDR          0x02180000 /*   4 pages */
#define AIPS2_CONFIG_PADDR        0x0217C000 /*   4 pages */
#define ARM_MPCORE_PADDR          0x02161000 /*  27 pages */
#define PLATFORM_CONTROL_PADDR    0x02160000 /*   1 page  */
#define PTM3_PADDR                0x0215F000 /*   1 page  */
#define PTM2_PADDR                0x0215E000 /*   1 page  */
#define PTM1_PADDR                0x0215D000 /*   1 page  */
#define PTM0_PADDR                0x0215C000 /*   1 page  */
#define CTI3_PADDR                0x0215B000 /*   1 page  */
#define CTI2_PADDR                0x0215A000 /*   1 page  */
#define CTI1_PADDR                0x02159000 /*   1 page  */
#define CTI0_PADDR                0x02158000 /*   1 page  */
#define CPU3_PMU_PADDR            0x02157000 /*   1 page  */
#define CPU3_DEBUG_PADDR          0x02156000 /*   1 page  */
#define CPU2_PMU_PADDR            0x02155000 /*   1 page  */
#define CPU2_DEBUG_PADDR          0x02154000 /*   1 page  */
#define CPU1_PMU_PADDR            0x02153000 /*   1 page  */
#define CPU1_PADDR                0x02152000 /*   1 page  */
#define CPU0_PMU_PADDR            0x02151000 /*   1 page  */
#define CPU0_DEBUG_PADDR          0x02150000 /*   1 page  */
#define CA9_INTEG_PADDR           0x0214F000 /*   1 page  */
//#define RESERVED_PADDR          0x02145000 /*  10 pages */
#define FUNNEL_PADDR              0x02144000 /*   1 page  */
#define TPIU_PADDR                0x02143000 /*   1 page  */
#define CTI_PADDR                 0x02142000 /*   1 page  */
#define ETB_PADDR                 0x02141000 /*   1 page  */
#define DAP_ROM_TABLE_PADDR       0x02140000 /*   1 page  */
//#define RESERVED_PADDR          0x02110000 /*  48 pages */
#define CAAM_PADDR                0x02100000 /*  16 pages */
//#define RESERVED_PADDR          0x020FC000 /*   4 pages */
//#define RESERVED_PADDR          0x020F8000 /*   4 pages */
//#define RESERVED_PADDR          0x020F4000 /*   4 pages */
//#define RESERVED_PADDR          0x020F0000 /*   4 pages */
#define SDMA_PADDR                0x020EC000 /*   4 pages */
#define DCIC2_PADDR               0x020E8000 /*   4 pages */
#define DCIC1_PADDR               0x020E4000 /*   4 pages */
#define IOMUXC_PADDR              0x020E0000 /*   4 pages */
//#define RESERVED_PADDR          0x020DD000 /*   3 pages */
#define GPC_PADDR                 0x020DC000 /*   1 page  */
#define SRC_PADDR                 0x020D8000 /*   4 pages */
#define EPIT2_PADDR               0x020D4000 /*   4 pages */
#define EPIT1_PADDR               0x020D0000 /*   4 pages */
#define SNVS_HP_PADDR             0x020CC000 /*   4 pages */
//#define RESERVED_PADDR          0x020CB000 /*   1 page  */
#define USBPHY2_PADDR             0x020CA000 /*   1 page  */
#define USBPHY1_PADDR             0x020C9000 /*   1 page  */
#define ANALOG_PADDR              0x020C8000 /*   1 page  */
#define CCM_PADDR                 0x020C4000 /*   4 pages */
#define WDOG2_PADDR               0x020C0000 /*   4 pages */
#define WDOG1_PADDR               0x020BC000 /*   4 pages */
#define KPP_PADDR                 0x020B8000 /*   4 pages */
#define GPIO7_PADDR               0x020B4000 /*   4 pages */
#define GPIO6_PADDR               0x020B0000 /*   4 pages */
#define GPIO5_PADDR               0x020AC000 /*   4 pages */
#define GPIO4_PADDR               0x020A8000 /*   4 pages */
#define GPIO3_PADDR               0x020A4000 /*   4 pages */
#define GPIO2_PADDR               0x020A0000 /*   4 pages */
#define GPIO1_PADDR               0x0209C000 /*   4 pages */
#define GPT_PADDR                 0x02098000 /*   4 pages */
#define CAN2_PADDR                0x02094000 /*   4 pages */
#define CAN1_PADDR                0x02090000 /*   4 pages */
#define PWM4_PADDR                0x0208C000 /*   4 pages */
#define PWM3_PADDR                0x02088000 /*   4 pages */
#define PWM2_PADDR                0x02084000 /*   4 pages */
#define PWM1_PADDR                0x02080000 /*   4 pages */
#define AIPS1_CONFIG_PADDR        0x0207C000 /*   4 pages */
#define VPU_PADDR                 0x02040000 /*  60 pages */
#define AIPS1_SPBA_PADDR          0x0203C000 /*   4 pages */
//#define RESERVED_FOR_SDMA_PADDR 0x02038000 /*   4 pages */
#define ASRC_PADDR                0x02034000 /*   4 pages */
#define SSI3_PADDR                0x02030000 /*   4 pages */
#define SSI2_PADDR                0x0202C000 /*   4 pages */
#define SSI1_PADDR                0x02028000 /*   4 pages */
#define ESAI_PADDR                0x02024000 /*   4 pages */
#define UART1_PADDR               0x02020000 /*   4 pages */
//#define RESERVED_FOR_SDMA_PADDR 0x0201C000 /*   4 pages */
#define ECSPI5_PADDR              0x02018000 /*   4 pages */
#define ECSPI4_PADDR              0x02014000 /*   4 pages */
#define ECSPI3_PADDR              0x02010000 /*   4 pages */
#define ECSPI2_PADDR              0x0200C000 /*   4 pages */
#define ECSPI1_PADDR              0x02008000 /*   4 pages */
#define SPDIF_PADDR               0x02004000 /*   4 pages */
//#define RESERVED_FOR_SDMA_PADDR 0x02000000 /*   4 pages */
#define PCIE_REGISTERS_PADDR      0x01FFC000 /*   4 pages */
#define PCIE_PADDR                0x01000000 /*  15 MB    */
//#define RESERVED_PADDR          0x00D00000 /*   3 MB    */
#define GPV1_PL301_CONFIG_PADDR   0x00C00000 /*   1 MB    */
#define GPV0_PL301_CONFIG_PADDR   0x00B00000 /*   1 MB    */
//#define RESERVED_PADDR          0x00A03000 /* 253 pages */
#define L2CC_PL310_PADDR          0x00A02000 /*   1 page  */
#define ARM_MP_PADDR              0x00A00000 /*   2 pages */
#define OCRAM_ALIASED_PADDR       0x00940000 /* 192 pages */
#define OCRAM_PADDR               0x00900000 /*  64 pages */
#define GPV4_PL301_CONFIG_PADDR   0x00800000 /*   1 MB    */
//#define RESERVED_PADDR          0x00400000 /*   4 MB    */
#define GPV3_PL301_CONFIG_PADDR   0x00300000 /*   1 MB    */
#define GPV2_PL301_CONFIG_PADDR   0x00200000 /*   1 MB    */
//#define RESERVED_PADDR          0x0013C000 /* 196 pages */
#define DTCP_PADDR                0x00138000 /*   4 pages */
#define GPU2D_PADDR               0x00134000 /*   4 pages */
#define GPU3D_PADDR               0x00130000 /*   4 pages */
//#define RESERVED_PADDR          0x00129000 /*   7 pages */
#define HDMI_PADDR                0x00120000 /*   9 pages */
//#define RESERVED_PADDR          0x00118000 /*   8 pages */
#define BCH_PADDR                 0x00114000 /*   4 pages */
#define GPMI_PADDR                0x00112000 /*   2 pages */
#define APBH_BRIDGE_DMA_PADDR     0x00110000 /*   2 pages */
//#define RESERVED_PADDR          0x00104000 /*  12 pages */
#define CAAM_SECURE_RAM_PADDR     0x00100000 /*   4 pages */
//#define RESERVED_PADDR          0x00018000 /* 232 pages */
#define BOOT_ROM_PADDR            0x00000000 /*  24 pages */


#endif /* !__PLAT_MACHINE_DEVICES_H */
