/*
 * Copyright 2017, DornerWorks
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_DORNERWORKS_GPL)
 */
/*
 *
 * This data was produced by DornerWorks, Ltd. of Grand Rapids, MI, USA under
 * a DARPA SBIR, Contract Number D16PC00107.
 *
 * Approved for Public Release, Distribution Unlimited.
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

#include <plat_mode/machine/devices.h>

#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

/* These devices are used by the seL4 kernel. */
#define UART_PADDR                  UART0_PADDR

#define ACPU_GIC_PADDR              0xF9000000
#define ACPU_GIC_DISTRIBUTOR_PADDR  0xF9010000
#define ACPU_GIC_CONTROLLER_PADDR   0xF9020000
#define AMS_CTRL_PADDR              0xFFA50000
#define AMS_PL_SYSMON_PADDR         0xFFA50C00
#define AMS_PS_SYSMON_PADDR         0xFFA50800
#define APM_CCI_INTC_PADDR          0xFD490000
#define APM_DDR_PADDR               0xFD0B0000
#define APM_INTC_OCM_PADDR          0xFFA00000
#define APM_LPD_FPD_PADDR           0xFFA10000
#define APU_PADDR                   0xFD5C0000
#define AXIPCIE_DMA0_PADDR          0xFD0F0000
#define AXIPCIE_DMA1_PADDR          0xFD0F0080
#define AXIPCIE_DMA2_PADDR          0xFD0F0100
#define AXIPCIE_DMA3_PADDR          0xFD0F0180
#define AXIPCIE_EGRESS0_PADDR       0xFD0E0C00
#define AXIPCIE_EGRESS1_PADDR       0xFD0E0C20
#define AXIPCIE_EGRESS2_PADDR       0xFD0E0C40
#define AXIPCIE_EGRESS3_PADDR       0xFD0E0C60
#define AXIPCIE_EGRESS4_PADDR       0xFD0E0C80
#define AXIPCIE_EGRESS5_PADDR       0xFD0E0CA0
#define AXIPCIE_EGRESS6_PADDR       0xFD0E0CC0
#define AXIPCIE_EGRESS7_PADDR       0xFD0E0CE0
#define AXIPCIE_INGRESS0_PADDR      0xFD0E0800
#define AXIPCIE_INGRESS1_PADDR      0xFD0E0820
#define AXIPCIE_INGRESS2_PADDR      0xFD0E0840
#define AXIPCIE_INGRESS3_PADDR      0xFD0E0860
#define AXIPCIE_INGRESS4_PADDR      0xFD0E0880
#define AXIPCIE_INGRESS5_PADDR      0xFD0E08A0
#define AXIPCIE_INGRESS6_PADDR      0xFD0E08C0
#define AXIPCIE_INGRESS7_PADDR      0xFD0E08E0
#define AXIPCIE_MAIN_PADDR          0xFD0E0000
#define BBRAM_PADDR                 0xFFCD0000
#define CAN0_PADDR                  0xFF060000
#define CAN1_PADDR                  0xFF070000
#define CCI_GPV_PADDR               0xFD6E0000
#define CCI_REG_PADDR               0xFD5E0000
#define CORESIGHT_A53_CTI_0_PADDR   0xFEC20000
#define CORESIGHT_A53_CTI_1_PADDR   0xFED20000
#define CORESIGHT_A53_CTI_2_PADDR   0xFEE20000
#define CORESIGHT_A53_CTI_3_PADDR   0xFEF20000
#define CORESIGHT_A53_DBG_0_PADDR   0xFEC10000
#define CORESIGHT_A53_DBG_1_PADDR   0xFED10000
#define CORESIGHT_A53_DBG_2_PADDR   0xFEE10000
#define CORESIGHT_A53_DBG_3_PADDR   0xFEF10000
#define CORESIGHT_A53_ETM_0_PADDR   0xFEC40000
#define CORESIGHT_A53_ETM_1_PADDR   0xFED40000
#define CORESIGHT_A53_ETM_2_PADDR   0xFEE40000
#define CORESIGHT_A53_ETM_3_PADDR   0xFEF40000
#define CORESIGHT_A53_PMU_0_PADDR   0xFEC30000
#define CORESIGHT_A53_PMU_1_PADDR   0xFED30000
#define CORESIGHT_A53_PMU_2_PADDR   0xFEE30000
#define CORESIGHT_A53_PMU_3_PADDR   0xFEF30000
#define CORESIGHT_A53_ROM_PADDR     0xFEC00000
#define CORESIGHT_R5_CTI_0_PADDR    0xFEBF8000
#define CORESIGHT_R5_CTI_1_PADDR    0xFEBF9000
#define CORESIGHT_R5_DBG_0_PADDR    0xFEBF0000
#define CORESIGHT_R5_DBG_1_PADDR    0xFEBF2000
#define CORESIGHT_R5_ETM_0_PADDR    0xFEBFC000
#define CORESIGHT_R5_ETM_1_PADDR    0xFEBFD000
#define CORESIGHT_R5_ROM_PADDR      0xFEBE0000
#define CORESIGHT_SOC_CTI_0_PADDR   0xFE990000
#define CORESIGHT_SOC_CTI_1_PADDR   0xFE9A0000
#define CORESIGHT_SOC_CTI_2_PADDR   0xFE9B0000
#define CORESIGHT_SOC_ETF_1_PADDR   0xFE940000
#define CORESIGHT_SOC_ETF_2_PADDR   0xFE950000
#define CORESIGHT_SOC_ETR_PADDR     0xFE970000
#define CORESIGHT_SOC_FTM_PADDR     0xFE9D0000
#define CORESIGHT_SOC_FUNN_0_PADDR  0xFE910000
#define CORESIGHT_SOC_FUNN_1_PADDR  0xFE920000
#define CORESIGHT_SOC_FUNN_2_PADDR  0xFE930000
#define CORESIGHT_SOC_REPLIC_PADDR  0xFE960000
#define CORESIGHT_SOC_ROM_PADDR     0xFE800000
#define CORESIGHT_SOC_STM_PADDR     0xFE9C0000
#define CORESIGHT_SOC_TPIU_PADDR    0xFE980000
#define CORESIGHT_SOC_TSGEN_PADDR   0xFE900000
#define CRF_APB_PADDR               0xFD1A0000
#define CRL_APB_PADDR               0xFF5E0000
#define CSU_PADDR                   0xFFCA0000
#define CSUDMA_PADDR                0xFFC80000
#define CSU_WDT_PADDR               0xFFCB0000
#define DDRC_PADDR                  0xFD070000
#define DDR_PHY_PADDR               0xFD080000
#define DDR_QOS_CTRL_PADDR          0xFD090000
#define DDR_XMPU0_CFG_PADDR         0xFD000000
#define DDR_XMPU1_CFG_PADDR         0xFD010000
#define DDR_XMPU2_CFG_PADDR         0xFD020000
#define DDR_XMPU3_CFG_PADDR         0xFD030000
#define DDR_XMPU4_CFG_PADDR         0xFD040000
#define DDR_XMPU5_CFG_PADDR         0xFD050000
#define DP_PADDR                    0xFD4A0000
#define DPDMA_PADDR                 0xFD4C0000
#define EFUSE_PADDR                 0xFFCC0000
#define FPD_DMA_CH0_PADDR           0xFD500000
#define FPD_DMA_CH1_PADDR           0xFD510000
#define FPD_DMA_CH2_PADDR           0xFD520000
#define FPD_DMA_CH3_PADDR           0xFD530000
#define FPD_DMA_CH4_PADDR           0xFD540000
#define FPD_DMA_CH5_PADDR           0xFD550000
#define FPD_DMA_CH6_PADDR           0xFD560000
#define FPD_DMA_CH7_PADDR           0xFD570000
#define FPD_GPV_PADDR               0xFD700000
#define FPD_SLCR_PADDR              0xFD610000
#define FPD_SLCR_SECURE_PADDR       0xFD690000
#define FPD_XMPU_CFG_PADDR          0xFD5D0000
#define FPD_XMPU_SINK_PADDR         0xFD4F0000
#define GEM0_PADDR                  0xFF0B0000
#define GEM1_PADDR                  0xFF0C0000
#define GEM2_PADDR                  0xFF0D0000
#define GEM3_PADDR                  0xFF0E0000
#define GPIO_PADDR                  0xFF0A0000
#define GPU_PADDR                   0xFD4B0000
#define I2C0_PADDR                  0xFF020000
#define I2C1_PADDR                  0xFF030000
#define IOU_GPV_PADDR               0xFE000000
#define IOU_SCNTR_PADDR             0xFF250000
#define IOU_SCNTRS_PADDR            0xFF260000
#define IOU_SECURE_SLCR_PADDR       0xFF240000
#define IOU_SLCR_PADDR              0xFF180000
#define IPI_PADDR                   0xFF300000
#define LPD_DMA_CH0_PADDR           0xFFA80000
#define LPD_DMA_CH1_PADDR           0xFFA90000
#define LPD_DMA_CH2_PADDR           0xFFAA0000
#define LPD_DMA_CH3_PADDR           0xFFAB0000
#define LPD_DMA_CH4_PADDR           0xFFAC0000
#define LPD_DMA_CH5_PADDR           0xFFAD0000
#define LPD_DMA_CH6_PADDR           0xFFAE0000
#define LPD_DMA_CH7_PADDR           0xFFAF0000
#define LPD_GPV_PADDR               0xFE100000
#define LPD_SLCR_PADDR              0xFF410000
#define LPD_SLCR_SECURE_PADDR       0xFF4B0000
#define LPD_XPPU_PADDR              0xFF980000
#define LPD_XPPU_SINK_PADDR         0xFF9C0000
#define NAND_PADDR                  0xFF100000
#define OCM_PADDR                   0xFF960000
#define OCM_XMPU_CFG_PADDR          0xFFA70000
#define PCIE_ATTRIB_PADDR           0xFD480000
#define PMU_GLOBAL_PADDR            0xFFD80000
#define QSPI_PADDR                  0xFF0F0000
#define RCPU_GIC_PADDR              0xF9000000
#define RPU_PADDR                   0xFF9A0000
#define RSA_PADDR                   0xFFCE002C
#define RSA_CORE_PADDR              0xFFCE0000
#define RTC_PADDR                   0xFFA60000
#define SATA_AHCI_HBA_PADDR         0xFD0C0000
#define SATA_AHCI_PORT0_CNTRL_PADDR 0xFD0C0100
#define SATA_AHCI_PORT1_CNTRL_PADDR 0xFD0C0180
#define SATA_AHCI_VENDOR_PADDR      0xFD0C00A0
#define SD0_PADDR                   0xFF160000
#define SD1_PADDR                   0xFF170000
#define SERDES_PADDR                0xFD400000
#define SIOU_PADDR                  0xFD3D0000
#define SMMU_GPV_PADDR              0xFD800000
#define SMMU_REG_PADDR              0xFD5F0000
#define SPI0_PADDR                  0xFF040000
#define SPI1_PADDR                  0xFF050000
#define SWDT_PADDR                  0xFF150000
#define S_AXI_HPC_0_FPD_PADDR       0xFD360000
#define S_AXI_HPC_1_FPD_PADDR       0xFD370000
#define S_AXI_HP_0_FPD_PADDR        0xFD380000
#define S_AXI_HP_1_FPD_PADDR        0xFD390000
#define S_AXI_HP_2_FPD_PADDR        0xFD3A0000
#define S_AXI_HP_3_FPD_PADDR        0xFD3B0000
#define S_AXI_LPD_PADDR             0xFF9B0000
#define TTC0_PADDR                  0xFF110000
#define TTC1_PADDR                  0xFF120000
#define TTC2_PADDR                  0xFF130000
#define TTC3_PADDR                  0xFF140000
#define UART0_PADDR                 0xFF000000
#define UART1_PADDR                 0xFF010000
#define USB3_0_PADDR                0xFF9D0000
#define USB3_0_XHCI_PADDR           0xFE200000
#define USB3_1_PADDR                0xFF9E0000
#define USB3_1_XHCI_PADDR           0xFE300000
#define VCU_PL_DECODE_PADDR         0xA0020000
#define VCU_PL_ENCODE_PADDR         0xA0000000
#define VCU_PL_SLCR_PADDR           0xA0040000
#define WDT_PADDR                   0xFD4D0000

#endif /* !__PLAT_MACHINE_DEVICES_H */
