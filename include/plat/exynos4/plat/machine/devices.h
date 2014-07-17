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
#define UART1_PPTR                  0xfff01000
#define MCT_PPTR                    0xfff02000
#define L2CC_PPTR                   0xfff03000
#define GIC_CONTROLLER_PPTR         0xfff04000
#define GIC_DISTRIBUTOR_PPTR        0xfff05000

#define L2CC_L2C310_PPTR            L2CC_PPTR
#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define EXYNOS_MCT_MHZ              24
#define EXYNOS_MCT_PPTR             MCT_PPTR

/* Other devices on the SoC */
#define CHIPID_PADDR                0x10000000
#define SYSREG_PADDR                0x10010000
#define PMU_PADDR                   0x10020000
#define CMU_TOP_PART_PADDR          0x10030000
#define CMU_DMC_PART_PADDR          0x10040000
#define MCT_PADDR                   0x10050000
#define WDT_PADDR                   0x10060000
#define RTC_PADDR                   0x10070000
#define KEYIF_PADDR                 0x100A0000
#define HDMI_CEC_PADDR              0x100B0000
#define TMU_PADDR                   0x100C0000
#define SECKEY_PADDR                0x10100000
#define TZPC0_PADDR                 0x10110000
#define TZPC1_PADDR                 0x10120000
#define TZPC2_PADDR                 0x10130000
#define TZPC3_PADDR                 0x10140000
#define TZPC4_PADDR                 0x10150000
#define TZPC5_PADDR                 0x10160000
#define DMC0_PADDR                  0x10400000
#define DMC1_PADDR                  0x10410000
#define INT_COMBINER_PADDR          0x10440000
#define IEM_IEC_PADDR               0x10460000
#define IEM_APC_PADDR               0x10470000
#define GIC_CONTROLLER0_PADDR       0x10480000
#define GIC_CONTROLLER1_PADDR       0x10488000
#define GIC_DISTRIBUTOR_PADDR       0x10490000
#define CORE_TIMERS_PADDR           0x104C0000
#define MPCORE_PRIV_REG_PADDR       0x10500000
#define L2CC_PADDR                  0x10502000
#define AXI_DMCD_PADDR              0x10600000
#define AXI_DMCSFRX_PADDR           0x10620000
#define ASYNCAXI_GDL_DMCD_PADDR     0x10640000
#define ASYNCAXI_GDR_DMCD_PADDR     0x10650000
#define QECPU_PADDR                 0x10680000
#define SMDMA0_PADDR                0x10800000
#define NSMDMA0_PADDR               0x10810000
#define SSS_PADDR                   0x10830000
#define CORESIGHT_PADDR             0x10880000
#define AXI_ACPX_PADDR              0x10A00000
#define SMMUMDMA_PADDR              0x10A40000
#define SMMUSSS_PADDR               0x10A50000
#define QEMDMA_PADDR                0x10AA0000
#define QESSS_PADDR                 0x10AB0000
#define AXI_MAUDIOX_PADDR           0x10E00000
#define GPIO_RIGHT_PADDR            0x11000000
#define AXI_GDR_PADDR               0x11200000
#define AXI_GPR_PADDR               0x11210000
#define ASYNCAXI_CAMIF_PADDR        0x11240000
#define ASYNCAXI_LCD0_PADDR         0x11250000
#define ASYNCAXI_LCD1_PADDR         0x11260000
#define ASYNCAXI_FSYSD_PADDR        0x11270000
#define ASYNCAXI_MAUDIO_PADDR       0x11290000
#define GPIO_LEFT_PADDR             0x11400000
#define AXI_GDL_PADDR               0x11600000
#define AXI_GPL_PADDR               0x11610000
#define ASYNCAXI_IMAGE_PADDR        0x11640000
#define ASYNCAXI_TV_PADDR           0x11650000
#define ASYNCAXI_MFC_L_PADDR        0x11660000
#define FIMC0_PADDR                 0x11800000
#define FIMC1_PADDR                 0x11810000
#define FIMC2_PADDR                 0x11820000
#define FIMC3_PADDR                 0x11830000
#define JPEG_PADDR                  0x11840000
#define MIPI_CSI0_PADDR             0x11880000
#define MIPI_CSI1_PADDR             0x11890000
#define AXI_CAMX_PADDR              0x11A00000
#define SMMUFIMC0_PADDR             0x11A20000
#define SMMUFIMC1_PADDR             0x11A30000
#define SMMUFIMC2_PADDR             0x11A40000
#define SMMUFIMC3_PADDR             0x11A50000
#define SMMUJPEG_PADDR              0x11A60000
#define QEFIMC0_PADDR               0x11A70000
#define QEFIMC1_PADDR               0x11A80000
#define QEFIMC2_PADDR               0x11A90000
#define QEFIMC3_PADDR               0x11AA0000
#define FIMD0_PADDR                 0x11C00000
#define MIE0_PADDR                  0x11C20000
#define MIPI_DSI0_PADDR             0x11C80000
#define AXI_LCD0_PADDR              0x11E00000
#define SMMUFIMD0_PADDR             0x11E20000
#define FIMD1_PADDR                 0x12000000
#define MIE1_PADDR                  0x12020000
#define MIPI_DSI1_PADDR             0x12080000
#define AXI_LCD1X_PADDR             0x12200000
#define SMMUFIMD1_PADDR             0x12220000
#define PPMU_LCD1_PADDR             0x12240000
#define PCIE_PADDR                  0x12400000
#define USBDEV_LINK_PADDR           0x12480000
#define TSI_PADDR                   0x12500000
#define SDMMC0_PADDR                0x12510000
#define SDMMC1_PADDR                0x12520000
#define SDMMC2_PADDR                0x12530000
#define SDMMC3_PADDR                0x12540000
#define SDMMC4_PADDR                0x12550000
#define SATA_PADDR                  0x12560000
#define SROMC_PADDR                 0x12570000
#define USBHOST_EHCI_PADDR          0x12580000
#define USBHOST_OHCI_PADDR          0x12590000
#define USBPHY_CON_PADDR            0x125B0000
#define PCIEPHY_PADDR               0x125C0000
#define SATA_PHY_PADDR              0x125D0000
#define AXI_FSYSD_PADDR             0x12600000
#define AXI_FSYSS_PADDR             0x12610000
#define SMMUPCIE_PADDR              0x12620000
#define PDMA0_PADDR                 0x12680000
#define PDMA1_PADDR                 0x12690000
#define PCIEPHY_CTRL_PADDR          0x126A0000
#define SATAPHY_CTRL_PADDR          0x126B0000
#define SMDMA2_PADDR                0x12840000
#define NSMDMA2_PADDR               0x12850000
#define AXI_IMGX_PADDR              0x12A00000
#define SMMUG2D_PADDR               0x12A20000
#define SMMUROTATOR_PADDR           0x12A30000
#define SMMUMDMA2_PADDR             0x12A40000
#define QEG2D_PADDR                 0x12A60000
#define QEROTATOR_PADDR             0x12A70000
#define QEMDMA2_PADDR               0x12A80000
#define VP_PADDR                    0x12C00000
#define MIXER_PADDR                 0x12C10000
#define TVENC_PADDR                 0x12C20000
#define HDMI0_PADDR                 0x12D00000
#define HDMI1_PADDR                 0x12D10000
#define HDMI2_PADDR                 0x12D20000
#define HDMI3_PADDR                 0x12D30000
#define HDMI4_PADDR                 0x12D40000
#define HDMI5_PADDR                 0x12D50000
#define HDMI6_PADDR                 0x12D60000
#define AXI_TVX_PADDR               0x12E00000
#define SMMUTV_PADDR                0x12E20000
#define SMMUMFC_L_PADDR             0x13620000
#define SMMUMFC_R_PADDR             0x13630000
#define UART0_PADDR                 0x13800000
#define UART1_PADDR                 0x13810000
#define UART2_PADDR                 0x13820000
#define UART3_PADDR                 0x13830000
#define UART4_PADDR                 0x13840000
#define I2C0_PADDR                  0x13860000
#define I2C1_PADDR                  0x13870000
#define I2C2_PADDR                  0x13880000
#define I2C3_PADDR                  0x13890000
#define I2C4_PADDR                  0x138A0000
#define I2C5_PADDR                  0x138B0000
#define I2C6_PADDR                  0x138C0000
#define I2C7_PADDR                  0x138D0000
#define I2CHDMI_PADDR               0x138E0000
#define TSADC_PADDR                 0x13910000
#define SPI0_PADDR                  0x13920000
#define SPI1_PADDR                  0x13930000
#define SPI2_PADDR                  0x13940000
#define I2S1_PADDR                  0x13960000
#define I2S2_PADDR                  0x13970000
#define PCM1_PADDR                  0x13980000
#define PCM2_PADDR                  0x13990000
#define AC97_PADDR                  0x139A0000
#define SPDIF_PADDR                 0x139B0000
#define PWMTIMER_PADDR              0x139D0000
#define MODEMIF_PADDR               0x13A00000

#endif /* !__PLAT_MACHINE_DEVICES_H */
