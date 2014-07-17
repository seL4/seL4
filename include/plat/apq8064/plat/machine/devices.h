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
#define UART_PPTR                   0xfff01000
#define TIMER_PPTR                  0xfff02000
#define GIC_DISTRIBUTOR_PPTR        0xfff04000
#define GIC_CONTROLLER_PPTR         0xfff05000

#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define GIC_DISTRIBUTOR_PADDR       APCS_QGIC2_PADDR + 0x0000
#define GIC_CONTROLLER0_PADDR       APCS_QGIC2_PADDR + 0x2000

#define UART_PADDR                  GSBI7_UART_DM_PADDR
#define TIMER_PADDR                 APCS_TMR_PADDR

/* Other devices on the SoC */
#define RPM_PADDR                 0x00000000 /* RPM */
#define RPM_TIMERS_PADDR          0x00062000 /* RPM */
#define RPM_MSG_RAM_XPU_PADDR     0x00100000 /* RPM */
#define MPM_PADDR                 0x00200000 /* RPM */
#define PA1_SSBI2_CFG_PADDR       0x00300000 /* RPM */
#define PA1_XPU_PADDR             0x00400000 /* RPM */
#define PA1_SSBI2_CMD_PADDR       0x00500000 /* RPM */
#define PA2_SSBI2_CFG_PADDR       0x00600000 /* RPM */
#define SEC_CTRL_PADDR            0x00700000 /* Security */
#define TLMM_PADDR                0x00800000 /* chip_core_top */
#define CLK_CTL_PADDR             0x00900000 /* Clock_Controller */
#define EBI1_CH0_PADDR            0x00A00000 /* LPDDR */
#define SYS_IMEM_PADDR            0x00B00000 /* chip_core_top */
#define PA2_SSBI2_CMD_PADDR       0x00C00000 /* RPM */
#define EBI1_CH1_PADDR            0x00D00000 /* LPDDR */
#define SFPB_WRAPPER_XPU_PADDR    0x00E00000 /* SFPB_Wrapper */
#define SFPB_WRAPPER_PADDR        0x00F00000 /* SFPB_Wrapper */
#define SPDM_PADDR                0x01000000 /* chip_core_top */
#define SPDM_SECURE_PADDR         0x01100000 /* chip_core_top */
#define SFPB_WRAPPER_MUTEX_PADDR  0x01200000 /* SFPB_Wrapper */
#define SFAB_PADDR                0x01300000 /* Fabrics */
#define AFAB_PADDR                0x01400000 /* Fabrics */
#define DAY_CFG_PADDR             0x01500000 /* Fabrics */
#define SFPB_WRAPPER_1x2_PADDR    0x01600000 /* SFPB_Wrapper */
#define SFPB_WRAPPER_2x1_PADDR    0x01800000 /* SFPB_Wrapper */
#define QDSS_DAPROM_PADDR         0x01A00000 /* Debug_Sub_System */
#define QDSS_ETB_PADDR            0x01A01000 /* Debug_Sub_System */
#define QDSS_CTI1_PADDR           0x01A02000 /* Debug_Sub_System */
#define QDSS_TPIU_PADDR           0x01A03000 /* Debug_Sub_System */
#define QDSS_TFUNNEL_PADDR        0x01A04000 /* Debug_Sub_System */
#define QDSS_ITM_PADDR            0x01A05000 /* Debug_Sub_System */
#define QDSS_STM_PADDR            0x01A06000 /* Debug_Sub_System */
#define QDSS_DAPM2VMT_PADDR       0x01A80000 /* Debug_Sub_System */
#define APCS_QGIC2_PADDR          0x02000000 /* KraitMP_Sub_System */
#define APCS_ACC_PADDR            0x02008000 /* KraitMP_Sub_System */
#define APCS_SAW2_PADDR           0x02009000 /* KraitMP_Sub_System */
#define APCS_TMR_PADDR            0x0200A000 /* KraitMP_Sub_System */
#define PCS_GLB_PADDR             0x02010000 /* KraitMP_Sub_System */
#define APCS_GCC_PADDR            0x02011000 /* KraitMP_Sub_System */
#define APCS_L2_GDHS_PADDR        0x02012000 /* KraitMP_Sub_System */
#define APCS_L2_MPU_PADDR         0x02013000 /* KraitMP_Sub_System */
#define CPU0_APCS_ACC_PADDR       0x02088000 /* KraitMP_Sub_System */
#define CPU0_APCS_SAW2_PADDR      0x02089000 /* KraitMP_Sub_System */
#define CPU0_APCS_TMR_PADDR       0x0208A000 /* KraitMP_Sub_System */
#define EXT_APCS_GLB_PADDR        0x02090000 /* KraitMP_Sub_System */
#define EXT_APCS_GCC_PADDR        0x02091000 /* KraitMP_Sub_System */
#define EXT_APCS_L2_GDHS_PADDR    0x02092000 /* KraitMP_Sub_System */
#define EXT_APCS_L2_MPU_PADDR     0x02093000 /* KraitMP_Sub_System */
#define CPU1_APCS_ACC_PADDR       0x02098000 /* KraitMP_Sub_System */
#define CPU1_APCS_SAW2_PADDR      0x02099000 /* KraitMP_Sub_System */
#define CPU1_APCS_TMR_PADDR       0x0209A000 /* KraitMP_Sub_System */
#define CPU2_APCS_ACC_PADDR       0x020A8000 /* KraitMP_Sub_System */
#define CPU2_APCS_SAW2_PADDR      0x020A9000 /* KraitMP_Sub_System */
#define CPU2_APCS_TMR_PADDR       0x020AA000 /* KraitMP_Sub_System */
#define CPU3_APCS_ACC_PADDR       0x020B8000 /* KraitMP_Sub_System */
#define CPU3_APCS_SAW2_PADDR      0x020B9000 /* KraitMP_Sub_System */
#define CPU3_APCS_TMR_PADDR       0x020BA000 /* KraitMP_Sub_System */
#define APCS_HSEL_PADDR           0x02100000 /* KraitMP_Sub_System */
#define RIVA_PADDR                0x03000000 /* RIVA */
#define MMSS_CC_PADDR             0x04000000 /* Multi_Media_Sub_System */
#define OXILI_PADDR               0x04300000 /* Multi_Media_Sub_System */
#define MFC_PADDR                 0x04400000 /* Multi_Media_Sub_System */
#define VFE_PADDR                 0x04500000 /* Multi_Media_Sub_System */
#define GEMINI_PADDR              0x04600000 /* Multi_Media_Sub_System */
#define MIPI_DSI_1_PADDR          0x04700000 /* Multi_Media_Sub_System */
#define CSID_PADDR                0x04800000 /* Multi_Media_Sub_System */
#define HDMI_TX_PADDR             0x04A00000 /* Multi_Media_Sub_System */
#define IMEM_MMSS_PADDR           0x04B00000 /* Multi_Media_Sub_System */
#define ROTATOR_PADDR             0x04E00000 /* Multi_Media_Sub_System */
#define TV_ENC_PADDR              0x04F00000 /* Multi_Media_Sub_System */
#define JPEGD_PADDR               0x05000000 /* Multi_Media_Sub_System */
#define MDP_PADDR                 0x05100000 /* Multi_Media_Sub_System */
#define FABRIC_MMSS_PADDR         0x05200000 /* Fabrics */
#define VPE_PADDR                 0x05300000 /* Multi_Media_Sub_System */
#define MSS_APU_PADDR             0x05400000 /* Multi_Media_Sub_System */
#define MMSS_SFPB_CFG_PADDR       0x05700000 /* Multi_Media_Sub_System */
#define MIPI_DSI_2_PADDR          0x05800000 /* Multi_Media_Sub_System */
#define VCAP_PADDR                0x05900000 /* Multi_Media_Sub_System */
#define SMMU_VCAP_PADDR           0x07200000 /* Multi_Media_Sub_System */
#define SMMU_JPEGD_PADDR          0x07300000 /* Multi_Media_Sub_System */
#define SMMU_VPE_PADDR            0x07400000 /* Multi_Media_Sub_System */
#define SMMU_MDP4_0_PADDR         0x07500000 /* Multi_Media_Sub_System */
#define SMMU_MDP4_1_PADDR         0x07600000 /* Multi_Media_Sub_System */
#define SMMU_ROTATOR_PADDR        0x07700000 /* Multi_Media_Sub_System */
#define SMMU_JPEG_PADDR           0x07800000 /* Multi_Media_Sub_System */
#define SMMU_VFE_PADDR            0x07900000 /* Multi_Media_Sub_System */
#define SMMU_SS1080P_0_PADDR      0x07A00000 /* Multi_Media_Sub_System */
#define SMMU_SS1080P_1_PADDR      0x07B00000 /* Multi_Media_Sub_System */
#define SMMU_GFX3D_PADDR          0x07C00000 /* Multi_Media_Sub_System */
#define SMMU_GFX3D1_PADDR         0x07D00000 /* Multi_Media_Sub_System */
#define SMMU_SFPB_CFG_DUMMY_PADDR 0x07F00000 /* Multi_Media_Sub_System */
#define GSS_A5_CSR_PADDR          0x10000000 /* GSS */
#define GSS_A5_SPM_PADDR          0x10001000 /* GSS */
#define GSS_A5_TIMERS_PADDR       0x10002000 /* GSS */
#define GSS_A5_SSBI_PADDR         0x10003000 /* GSS */
#define GSS_A5_QGIC2_PADDR        0x10008000 /* GSS */
#define NAV_PADDR                 0x10100000 /* GSS */
#define CE3_CRYPTO4_PADDR         0x11000000 /* Security */
#define PPSS_PADDR                0x12080000 /* Daytona_SPS */
#define PPSS_SLP_TIMERS_PADDR     0x12081000 /* Daytona_SPS */
#define SIC_PADDR                 0x120C0000 /* Daytona_SPS */
#define SIC_APU_PADDR             0x120C2000 /* Daytona_SPS */
#define SIC_NON_SECURE_PADDR      0x12100000 /* Daytona_SPS */
#define INTCTL0_PADDR             0x12100000 /* Daytona_SPS */
#define INTCTL1_PADDR             0x12100800 /* Daytona_SPS */
#define INTCTL2_PADDR             0x12101000 /* Daytona_SPS */
#define INTCTL3_PADDR             0x12101800 /* Daytona_SPS */
#define INTCTL4_PADDR             0x12102000 /* Daytona_SPS */
#define INTCTL5_PADDR             0x12102800 /* Daytona_SPS */
#define INTCTL6_PADDR             0x12103000 /* Daytona_SPS */
#define INTCTL7_PADDR             0x12103800 /* Daytona_SPS */
#define SDC2_PADDR                0x12140000 /* Daytona_SPS */
#define SDC2_DML_PADDR            0x12140800 /* Daytona_SPS */
#define SDC2_BAM_PADDR            0x12142000 /* Daytona_SPS */
#define SDC3_PADDR                0x12180000 /* Daytona_SPS */
#define SDC3_DML_PADDR            0x12180800 /* Daytona_SPS */
#define SDC3_BAM_PADDR            0x12182000 /* Daytona_SPS */
#define SDC4_PADDR                0x121C0000 /* Daytona_SPS */
#define SDC4_DML_PADDR            0x121C0800 /* Daytona_SPS */
#define SDC4_BAM_PADDR            0x121C2000 /* Daytona_SPS */
#define BAM_DMA_PADDR             0x12240000 /* Daytona_SPS */
#define BAM_DMA_BAM_PADDR         0x12244000 /* Daytona_SPS */
#define BAM_DMA_BAM_XPU_PADDR     0x12246000 /* Daytona_SPS */
#define SDC1_PADDR                0x12400000 /* Daytona_SPS */
#define SDC1_DML_PADDR            0x12400800 /* Daytona_SPS */
#define SDC1_BAM_PADDR            0x12402000 /* Daytona_SPS */
#define SPS_GSBI1_PADDR           0x12440000 /* Daytona_SPS */
#define SPS_UART1_DM_PADDR        0x12450000 /* Daytona_SPS */
#define SPS_QUP1_PADDR            0x12460000 /* Daytona_SPS */
#define SPS_GSBI2_PADDR           0x12480000 /* Daytona_SPS */
#define SPS_UART2_DM_PADDR        0x12490000 /* Daytona_SPS */
#define SPS_QUP2_PADDR            0x124A0000 /* Daytona_SPS */
#define USB1_HS_PADDR             0x12500000 /* Daytona_SPS */
#define USB1_HS_BAM_PADDR         0x12502000 /* Daytona_SPS */
#define USB2_HSIC_PADDR           0x12510000 /* Daytona_SPS */
#define USB3_HS_PADDR             0x12520000 /* Daytona_SPS */
#define USB3_HS_BAM_PADDR         0x12522000 /* Daytona_SPS */
#define USB4_HS_PADDR             0x12530000 /* Daytona_SPS */
#define USB4_HS_BAM_PADDR         0x12532000 /* Daytona_SPS */
#define CE2_CRYPTO4_PADDR         0x12560000 /* Security */
#define GSBI3_PADDR               0x16200000 /* GSBIs */
#define GSBI3_UART_DM_PADDR       0x16240000 /* GSBIs */
#define QUP3_PADDR                0x16280000 /* GSBIs */
#define GSBI4_PADDR               0x16300000 /* GSBIs */
#define GSBI4_UART_DM_PADDR       0x16340000 /* GSBIs */
#define QUP4_PADDR                0x16380000 /* GSBIs */
#define GSBI6_PADDR               0x16500000 /* GSBIs */
#define GSBI6_UART_DM_PADDR       0x16540000 /* GSBIs */
#define QUP6_PADDR                0x16580000 /* GSBIs */
#define GSBI7_PADDR               0x16600000 /* GSBIs */
#define GSBI7_UART_DM_PADDR       0x16640000 /* GSBIs */
#define QUP7_PADDR                0x16680000 /* GSBIs */
#define LPASS_XPU_PADDR           0x17000000 /* chip_core_top_XPUs */
#define KPSS_XPU_PADDR            0x17100000 /* chip_core_top_XPUs */
#define GSS_XPU_PADDR             0x17200000 /* chip_core_top_XPUs */
#define CFPB2_XPU_CFG_PADDR       0x17300000 /* chip_core_top_Peripherals */
#define CSYSFPB2_PADDR            0x17400000 /* chip_core_top_Peripherals */
#define CE3_XPU_CFG_PADDR         0x17500000 /* chip_core_top_XPUs */
#define CSYSFPB_SPL_PADDR         0x17F00000 /* chip_core_top_Peripherals */
#define USB1_FS_PADDR             0x18000000 /* chip_core_top_Peripherals */
#define TSIF_PADDR                0x18200000 /* chip_core_top_Peripherals */
#define TSPP_PADDR                0x18202000 /* chip_core_top_Peripherals */
#define TSIF_BAM_PADDR            0x18204000 /* chip_core_top_Peripherals */
#define ADM3_0_PADDR              0x18300000 /* chip_core_top_Peripherals */
#define CE1_CRYPTO4_PADDR         0x18500000 /* Security */
#define TSSC_PADDR                0x18600000 /* chip_core_top_Peripherals */
#define TSSC_SSBI_PADDR           0x18600000 /* chip_core_top_Peripherals */
#define MSM_PDM_PADDR             0x18700000 /* chip_core_top_Peripherals */
#define CSYSFPB_MST_PADDR         0x18D00000 /* chip_core_top_Peripherals */
#define CFPB1_XPU_CFG_PADDR       0x18E00000 /* chip_core_top_Peripherals */
#define CSYSFPB1_PADDR            0x18F00000 /* chip_core_top_Peripherals */
#define GSBI5_PADDR               0x1A200000 /* GSBIs */
#define GSBI5_UART_DM_PADDR       0x1A240000 /* GSBIs */
#define QUP5_PADDR                0x1A280000 /* GSBIs */
#define PMEM_PADDR                0x1A300000 /* chip_core_top_Peripherals */
#define MSM_TCSR_PADDR            0x1A400000 /* chip_core_top_Peripherals */
#define PRNG_PADDR                0x1A500000 /* chip_core_top_Peripherals */
#define DIM_D00_REG_PADDR         0x1A700000 /* LPDDR */
#define DIM_BD0_REG_PADDR         0x1A780000 /* EBI1_DIM */
#define DIM_D01_REG_PADDR         0x1A800000 /* LPDDR */
#define DIM_D02_REG_PADDR         0x1A900000 /* LPDDR */
#define DIM_D03_REG_PADDR         0x1AA00000 /* LPDDR */
#define DIM_C00_REG_PADDR         0x1AB00000 /* LPDDR */
#define DIM_BC0_REG_PADDR         0x1AB40000 /* EBI1_DIM */
#define DIM_C01_REG_PADDR         0x1AB80000 /* EBI1_DIM */
#define DIM_D10_REG_PADDR         0x1AC00000 /* LPDDR */
#define DIM_BD1_REG_PADDR         0x1AC80000 /* EBI1_DIM */
#define DIM_D11_REG_PADDR         0x1AD00000 /* LPDDR */
#define DIM_D12_REG_PADDR         0x1AE00000 /* LPDDR */
#define DIM_D13_REG_PADDR         0x1AF00000 /* LPDDR */
#define DIM_C10_REG_PADDR         0x1B000000 /* LPDDR */
#define DIM_BC1_REG_PADDR         0x1B040000 /* EBI1_DIM */
#define DIM_C11_REG_PADDR         0x1B080000 /* EBI1_DIM */
#define CFPB0_XPU_CFG_PADDR       0x1B100000 /* chip_core_top_Peripherals */
#define CSYSFPB0_PADDR            0x1B200000 /* chip_core_top_Peripherals */
#define SATA_XPU_CFG_PADDR        0x1B300000 /* chip_core_top_XPUs */
#define SATA_PHY_PADDR            0x1B400000 /* SATA */
#define PCIE20_PADDR              0x1B500000 /* PCIE20 */
#define PCIE20_ELBI_PADDR         0x1B502000 /* PCIE20 */
#define PCIE20_PARF_PADDR         0x1B600000 /* PCIE20 */
#define LPASS_CSR_PADDR           0x28000000 /* LP_Audio_Sub_System */
#define LPASS_M2VMT_PADDR         0x28002000 /* LP_Audio_Sub_System */
#define LPASS_M2VMT_Q6SS_PADDR    0x28003000 /* LP_Audio_Sub_System */
#define LPASS_AHBTM_PADDR         0x2800A000 /* LP_Audio_Sub_System */
#define LPASS_SLIMBUS_PADDR       0x28080000 /* LP_Audio_Sub_System */
#define LPASS_BAM_LITE_PADDR      0x28084000 /* LP_Audio_Sub_System */
#define LPA_IF_PADDR              0x28100000 /* LP_Audio_Sub_System */
#define MIDI_PADDR                0x28200000 /* LP_Audio_Sub_System */
#define LPASS_QDSP6SS_PUB_PADDR   0x28800000 /* LP_Audio_Sub_System */
#define LPASS_QDSP6SS_CSR_PADDR   0x28880000 /* LP_Audio_Sub_System */
#define LPASS_QDSP6SS_L2VIC_PADDR 0x28890000 /* LP_Audio_Sub_System */
#define LPASS_QDSP6SS_SAW_PADDR   0x288B0000 /* LP_Audio_Sub_System */
#define SATA_PADDR                0x29000000 /* SATA */

#endif /* !__PLAT_MACHINE_DEVICES_H */
