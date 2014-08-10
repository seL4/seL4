/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */
/* @AUTHOR(akroh@ertos.nicta.com.au) */

#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>


/* Available physical memory regions on platform (RAM) */
/* NOTE: Regions are not allowed to be adjacent! */
/*       and must be page-aligned                */
const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 2 GiB -1 page to prevent uin32_t overflow */
    { /* .start = */ 0x80000000, /* .end = */ 0xfffff000 }
};

BOOT_CODE int
get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t
get_avail_p_reg(unsigned int i)
{
    return avail_p_regs[i];
}
/*
 * We're limited to one page of boot data.
 * Exclude some devices.
 */
const p_region_t BOOT_RODATA dev_p_regs[] = {
    { /* .start */ RPM_PADDR                 , /* .end */ RPM_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ RPM_TIMERS_PADDR          , /* .end */ RPM_TIMERS_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ RPM_MSG_RAM_XPU_PADDR     , /* .end */ RPM_MSG_RAM_XPU_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ MPM_PADDR                 , /* .end */ MPM_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ PA1_SSBI2_CFG_PADDR       , /* .end */ PA1_SSBI2_CFG_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ PA1_XPU_PADDR             , /* .end */ PA1_XPU_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ PA1_SSBI2_CMD_PADDR       , /* .end */ PA1_SSBI2_CMD_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ PA2_SSBI2_CFG_PADDR       , /* .end */ PA2_SSBI2_CFG_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ SEC_CTRL_PADDR            , /* .end */ SEC_CTRL_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ TLMM_PADDR                , /* .end */ TLMM_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ CLK_CTL_PADDR             , /* .end */ CLK_CTL_PADDR             + (4 << PAGE_BITS) },
    { /* .start */ EBI1_CH0_PADDR            , /* .end */ EBI1_CH0_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SYS_IMEM_PADDR            , /* .end */ SYS_IMEM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ PA2_SSBI2_CMD_PADDR       , /* .end */ PA2_SSBI2_CMD_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ EBI1_CH1_PADDR            , /* .end */ EBI1_CH1_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SFPB_WRAPPER_XPU_PADDR    , /* .end */ SFPB_WRAPPER_XPU_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ SFPB_WRAPPER_PADDR        , /* .end */ SFPB_WRAPPER_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ SPDM_PADDR                , /* .end */ SPDM_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ SPDM_SECURE_PADDR         , /* .end */ SPDM_SECURE_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SFPB_WRAPPER_MUTEX_PADDR  , /* .end */ SFPB_WRAPPER_MUTEX_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SFAB_PADDR                , /* .end */ SFAB_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ AFAB_PADDR                , /* .end */ AFAB_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ DAY_CFG_PADDR             , /* .end */ DAY_CFG_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SFPB_WRAPPER_1x2_PADDR    , /* .end */ SFPB_WRAPPER_1x2_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ SFPB_WRAPPER_2x1_PADDR    , /* .end */ SFPB_WRAPPER_2x1_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ QDSS_DAPROM_PADDR         , /* .end */ QDSS_DAPROM_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ QDSS_ETB_PADDR            , /* .end */ QDSS_ETB_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ QDSS_CTI1_PADDR           , /* .end */ QDSS_CTI1_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ QDSS_TPIU_PADDR           , /* .end */ QDSS_TPIU_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ QDSS_TFUNNEL_PADDR        , /* .end */ QDSS_TFUNNEL_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ QDSS_ITM_PADDR            , /* .end */ QDSS_ITM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ QDSS_STM_PADDR            , /* .end */ QDSS_STM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ QDSS_DAPM2VMT_PADDR       , /* .end */ QDSS_DAPM2VMT_PADDR       + (1 << PAGE_BITS) },
//  { /* .start */ APCS_QGIC2_PADDR          , /* .end */ APCS_QGIC2_PADDR          + (3 << PAGE_BITS) },
    { /* .start */ APCS_ACC_PADDR            , /* .end */ APCS_ACC_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ APCS_SAW2_PADDR           , /* .end */ APCS_SAW2_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ APCS_TMR_PADDR            , /* .end */ APCS_TMR_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ PCS_GLB_PADDR             , /* .end */ PCS_GLB_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ APCS_GCC_PADDR            , /* .end */ APCS_GCC_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ APCS_L2_GDHS_PADDR        , /* .end */ APCS_L2_GDHS_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ APCS_L2_MPU_PADDR         , /* .end */ APCS_L2_MPU_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ CPU0_APCS_ACC_PADDR       , /* .end */ CPU0_APCS_ACC_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CPU0_APCS_SAW2_PADDR      , /* .end */ CPU0_APCS_SAW2_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ CPU0_APCS_TMR_PADDR       , /* .end */ CPU0_APCS_TMR_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ EXT_APCS_GLB_PADDR        , /* .end */ EXT_APCS_GLB_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ EXT_APCS_GCC_PADDR        , /* .end */ EXT_APCS_GCC_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ EXT_APCS_L2_GDHS_PADDR    , /* .end */ EXT_APCS_L2_GDHS_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ EXT_APCS_L2_MPU_PADDR     , /* .end */ EXT_APCS_L2_MPU_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ CPU1_APCS_ACC_PADDR       , /* .end */ CPU1_APCS_ACC_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CPU1_APCS_SAW2_PADDR      , /* .end */ CPU1_APCS_SAW2_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ CPU1_APCS_TMR_PADDR       , /* .end */ CPU1_APCS_TMR_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CPU2_APCS_ACC_PADDR       , /* .end */ CPU2_APCS_ACC_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CPU2_APCS_SAW2_PADDR      , /* .end */ CPU2_APCS_SAW2_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ CPU2_APCS_TMR_PADDR       , /* .end */ CPU2_APCS_TMR_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CPU3_APCS_ACC_PADDR       , /* .end */ CPU3_APCS_ACC_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CPU3_APCS_SAW2_PADDR      , /* .end */ CPU3_APCS_SAW2_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ CPU3_APCS_TMR_PADDR       , /* .end */ CPU3_APCS_TMR_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ APCS_HSEL_PADDR           , /* .end */ APCS_HSEL_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ RIVA_PADDR                , /* .end */ RIVA_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ MMSS_CC_PADDR             , /* .end */ MMSS_CC_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ OXILI_PADDR               , /* .end */ OXILI_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ MFC_PADDR                 , /* .end */ MFC_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ VFE_PADDR                 , /* .end */ VFE_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ GEMINI_PADDR              , /* .end */ GEMINI_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ MIPI_DSI_1_PADDR          , /* .end */ MIPI_DSI_1_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CSID_PADDR                , /* .end */ CSID_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ HDMI_TX_PADDR             , /* .end */ HDMI_TX_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ IMEM_MMSS_PADDR           , /* .end */ IMEM_MMSS_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ ROTATOR_PADDR             , /* .end */ ROTATOR_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ TV_ENC_PADDR              , /* .end */ TV_ENC_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ JPEGD_PADDR               , /* .end */ JPEGD_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ MDP_PADDR                 , /* .end */ MDP_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ FABRIC_MMSS_PADDR         , /* .end */ FABRIC_MMSS_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ VPE_PADDR                 , /* .end */ VPE_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ MSS_APU_PADDR             , /* .end */ MSS_APU_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ MMSS_SFPB_CFG_PADDR       , /* .end */ MMSS_SFPB_CFG_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ MIPI_DSI_2_PADDR          , /* .end */ MIPI_DSI_2_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ VCAP_PADDR                , /* .end */ VCAP_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ SMMU_VCAP_PADDR           , /* .end */ SMMU_VCAP_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SMMU_JPEGD_PADDR          , /* .end */ SMMU_JPEGD_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ SMMU_VPE_PADDR            , /* .end */ SMMU_VPE_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SMMU_MDP4_0_PADDR         , /* .end */ SMMU_MDP4_0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMU_MDP4_1_PADDR         , /* .end */ SMMU_MDP4_1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMU_ROTATOR_PADDR        , /* .end */ SMMU_ROTATOR_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ SMMU_JPEG_PADDR           , /* .end */ SMMU_JPEG_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SMMU_VFE_PADDR            , /* .end */ SMMU_VFE_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SMMU_SS1080P_0_PADDR      , /* .end */ SMMU_SS1080P_0_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ SMMU_SS1080P_1_PADDR      , /* .end */ SMMU_SS1080P_1_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ SMMU_GFX3D_PADDR          , /* .end */ SMMU_GFX3D_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ SMMU_GFX3D1_PADDR         , /* .end */ SMMU_GFX3D1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMU_SFPB_CFG_DUMMY_PADDR , /* .end */ SMMU_SFPB_CFG_DUMMY_PADDR + (1 << PAGE_BITS) },
    { /* .start */ GSS_A5_CSR_PADDR          , /* .end */ GSS_A5_CSR_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ GSS_A5_SPM_PADDR          , /* .end */ GSS_A5_SPM_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ GSS_A5_TIMERS_PADDR       , /* .end */ GSS_A5_TIMERS_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ GSS_A5_SSBI_PADDR         , /* .end */ GSS_A5_SSBI_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ GSS_A5_QGIC2_PADDR        , /* .end */ GSS_A5_QGIC2_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ NAV_PADDR                 , /* .end */ NAV_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ CE3_CRYPTO4_PADDR         , /* .end */ CE3_CRYPTO4_PADDR         + (1 << PAGE_BITS) },
    /* The timers here are mixed with IRQ config so we do not expose them */
    { /* .start */ PPSS_PADDR                , /* .end */ PPSS_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ PPSS_SLP_TIMERS_PADDR     , /* .end */ PPSS_SLP_TIMERS_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ SIC_PADDR                 , /* .end */ SIC_PADDR                 + (1 << PAGE_BITS) },
    { /* .start */ SIC_APU_PADDR             , /* .end */ SIC_APU_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SIC_NON_SECURE_PADDR      , /* .end */ SIC_NON_SECURE_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ INTCTL0_PADDR             , /* .end */ INTCTL0_PADDR             + (1 << PAGE_BITS) },
#if (INTCTL1_PADDR & 0xfff) == 0
    { /* .start */ INTCTL1_PADDR             , /* .end */ INTCTL1_PADDR             + (1 << PAGE_BITS) },
#endif
#if (INTCTL2_PADDR & 0xfff) == 0
    { /* .start */ INTCTL2_PADDR             , /* .end */ INTCTL2_PADDR             + (1 << PAGE_BITS) },
#endif
#if (INTCTL3_PADDR & 0xfff) == 0
    { /* .start */ INTCTL3_PADDR             , /* .end */ INTCTL3_PADDR             + (1 << PAGE_BITS) },
#endif
#if (INTCTL4_PADDR & 0xfff) == 0
    { /* .start */ INTCTL4_PADDR             , /* .end */ INTCTL4_PADDR             + (1 << PAGE_BITS) },
#endif
#if (INTCTL5_PADDR & 0xfff) == 0
    { /* .start */ INTCTL5_PADDR             , /* .end */ INTCTL5_PADDR             + (1 << PAGE_BITS) },
#endif
#if (INTCTL6_PADDR & 0xfff) == 0
    { /* .start */ INTCTL6_PADDR             , /* .end */ INTCTL6_PADDR             + (1 << PAGE_BITS) },
#endif
#if (INTCTL7_PADDR & 0xfff) == 0
    { /* .start */ INTCTL7_PADDR             , /* .end */ INTCTL7_PADDR             + (1 << PAGE_BITS) },
#endif
    { /* .start */ SDC2_PADDR                , /* .end */ SDC2_PADDR                + (1 << PAGE_BITS) },
#if (SDC2_DML_PADDR & 0xfff) == 0
    { /* .start */ SDC2_DML_PADDR            , /* .end */ SDC2_DML_PADDR            + (1 << PAGE_BITS) },
#endif
    { /* .start */ SDC2_BAM_PADDR            , /* .end */ SDC2_BAM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SDC3_PADDR                , /* .end */ SDC3_PADDR                + (1 << PAGE_BITS) },
#if (SDC3_DML_PADDR & 0xfff) == 0
    { /* .start */ SDC3_DML_PADDR            , /* .end */ SDC3_DML_PADDR            + (1 << PAGE_BITS) },
#endif
    { /* .start */ SDC3_BAM_PADDR            , /* .end */ SDC3_BAM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SDC4_PADDR                , /* .end */ SDC4_PADDR                + (1 << PAGE_BITS) },
#if (SDC4_DML_PADDR & 0xfff) == 0
    { /* .start */ SDC4_DML_PADDR            , /* .end */ SDC4_DML_PADDR            + (1 << PAGE_BITS) },
#endif
    { /* .start */ SDC4_BAM_PADDR            , /* .end */ SDC4_BAM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ BAM_DMA_PADDR             , /* .end */ BAM_DMA_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ BAM_DMA_BAM_PADDR         , /* .end */ BAM_DMA_BAM_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ BAM_DMA_BAM_XPU_PADDR     , /* .end */ BAM_DMA_BAM_XPU_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ SDC1_PADDR                , /* .end */ SDC1_PADDR                + (1 << PAGE_BITS) },
#if (SDC1_DML_PADDR & 0xfff) == 0
    { /* .start */ SDC1_DML_PADDR            , /* .end */ SDC1_DML_PADDR            + (1 << PAGE_BITS) },
#endif
    { /* .start */ SDC1_BAM_PADDR            , /* .end */ SDC1_BAM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SPS_GSBI1_PADDR           , /* .end */ SPS_GSBI1_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SPS_UART1_DM_PADDR        , /* .end */ SPS_UART1_DM_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ SPS_QUP1_PADDR            , /* .end */ SPS_QUP1_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SPS_GSBI2_PADDR           , /* .end */ SPS_GSBI2_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SPS_UART2_DM_PADDR        , /* .end */ SPS_UART2_DM_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ SPS_QUP2_PADDR            , /* .end */ SPS_QUP2_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ USB1_HS_PADDR             , /* .end */ USB1_HS_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ USB1_HS_BAM_PADDR         , /* .end */ USB1_HS_BAM_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ USB2_HSIC_PADDR           , /* .end */ USB2_HSIC_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ USB3_HS_PADDR             , /* .end */ USB3_HS_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ USB3_HS_BAM_PADDR         , /* .end */ USB3_HS_BAM_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ USB4_HS_PADDR             , /* .end */ USB4_HS_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ USB4_HS_BAM_PADDR         , /* .end */ USB4_HS_BAM_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ CE2_CRYPTO4_PADDR         , /* .end */ CE2_CRYPTO4_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ GSBI3_PADDR               , /* .end */ GSBI3_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ GSBI3_UART_DM_PADDR       , /* .end */ GSBI3_UART_DM_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ QUP3_PADDR                , /* .end */ QUP3_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ GSBI4_PADDR               , /* .end */ GSBI4_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ GSBI4_UART_DM_PADDR       , /* .end */ GSBI4_UART_DM_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ QUP4_PADDR                , /* .end */ QUP4_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ GSBI6_PADDR               , /* .end */ GSBI6_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ GSBI6_UART_DM_PADDR       , /* .end */ GSBI6_UART_DM_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ QUP6_PADDR                , /* .end */ QUP6_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ GSBI7_PADDR               , /* .end */ GSBI7_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ GSBI7_UART_DM_PADDR       , /* .end */ GSBI7_UART_DM_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ QUP7_PADDR                , /* .end */ QUP7_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ LPASS_XPU_PADDR           , /* .end */ LPASS_XPU_PADDR           + (1 << PAGE_BITS) },
//  { /* .start */ KPSS_XPU_PADDR            , /* .end */ KPSS_XPU_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ GSS_XPU_PADDR             , /* .end */ GSS_XPU_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ CFPB2_XPU_CFG_PADDR       , /* .end */ CFPB2_XPU_CFG_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CSYSFPB2_PADDR            , /* .end */ CSYSFPB2_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ CE3_XPU_CFG_PADDR         , /* .end */ CE3_XPU_CFG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ CSYSFPB_SPL_PADDR         , /* .end */ CSYSFPB_SPL_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ USB1_FS_PADDR             , /* .end */ USB1_FS_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ TSIF_PADDR                , /* .end */ TSIF_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ TSPP_PADDR                , /* .end */ TSPP_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ TSIF_BAM_PADDR            , /* .end */ TSIF_BAM_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ ADM3_0_PADDR              , /* .end */ ADM3_0_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ CE1_CRYPTO4_PADDR         , /* .end */ CE1_CRYPTO4_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ TSSC_PADDR                , /* .end */ TSSC_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ TSSC_SSBI_PADDR           , /* .end */ TSSC_SSBI_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ MSM_PDM_PADDR             , /* .end */ MSM_PDM_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ CSYSFPB_MST_PADDR         , /* .end */ CSYSFPB_MST_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ CFPB1_XPU_CFG_PADDR       , /* .end */ CFPB1_XPU_CFG_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CSYSFPB1_PADDR            , /* .end */ CSYSFPB1_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ GSBI5_PADDR               , /* .end */ GSBI5_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ GSBI5_UART_DM_PADDR       , /* .end */ GSBI5_UART_DM_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ QUP5_PADDR                , /* .end */ QUP5_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ PMEM_PADDR                , /* .end */ PMEM_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ MSM_TCSR_PADDR            , /* .end */ MSM_TCSR_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ PRNG_PADDR                , /* .end */ PRNG_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ DIM_D00_REG_PADDR         , /* .end */ DIM_D00_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_BD0_REG_PADDR         , /* .end */ DIM_BD0_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_D01_REG_PADDR         , /* .end */ DIM_D01_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_D02_REG_PADDR         , /* .end */ DIM_D02_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_D03_REG_PADDR         , /* .end */ DIM_D03_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_C00_REG_PADDR         , /* .end */ DIM_C00_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_BC0_REG_PADDR         , /* .end */ DIM_BC0_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_C01_REG_PADDR         , /* .end */ DIM_C01_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_D10_REG_PADDR         , /* .end */ DIM_D10_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_BD1_REG_PADDR         , /* .end */ DIM_BD1_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_D11_REG_PADDR         , /* .end */ DIM_D11_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_D12_REG_PADDR         , /* .end */ DIM_D12_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_D13_REG_PADDR         , /* .end */ DIM_D13_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_C10_REG_PADDR         , /* .end */ DIM_C10_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_BC1_REG_PADDR         , /* .end */ DIM_BC1_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DIM_C11_REG_PADDR         , /* .end */ DIM_C11_REG_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ CFPB0_XPU_CFG_PADDR       , /* .end */ CFPB0_XPU_CFG_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CSYSFPB0_PADDR            , /* .end */ CSYSFPB0_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SATA_XPU_CFG_PADDR        , /* .end */ SATA_XPU_CFG_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ SATA_PHY_PADDR            , /* .end */ SATA_PHY_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ PCIE20_PADDR              , /* .end */ PCIE20_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ PCIE20_ELBI_PADDR         , /* .end */ PCIE20_ELBI_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ PCIE20_PARF_PADDR         , /* .end */ PCIE20_PARF_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ LPASS_CSR_PADDR           , /* .end */ LPASS_CSR_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ LPASS_M2VMT_PADDR         , /* .end */ LPASS_M2VMT_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ LPASS_M2VMT_Q6SS_PADDR    , /* .end */ LPASS_M2VMT_Q6SS_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ LPASS_AHBTM_PADDR         , /* .end */ LPASS_AHBTM_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ LPASS_SLIMBUS_PADDR       , /* .end */ LPASS_SLIMBUS_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ LPASS_BAM_LITE_PADDR      , /* .end */ LPASS_BAM_LITE_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ LPA_IF_PADDR              , /* .end */ LPA_IF_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ MIDI_PADDR                , /* .end */ MIDI_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ LPASS_QDSP6SS_PUB_PADDR   , /* .end */ LPASS_QDSP6SS_PUB_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ LPASS_QDSP6SS_CSR_PADDR   , /* .end */ LPASS_QDSP6SS_CSR_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ LPASS_QDSP6SS_L2VIC_PADDR , /* .end */ LPASS_QDSP6SS_L2VIC_PADDR + (1 << PAGE_BITS) },
    { /* .start */ LPASS_QDSP6SS_SAW_PADDR   , /* .end */ LPASS_QDSP6SS_SAW_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ SATA_PADDR                , /* .end */ SATA_PADDR                + (1 << PAGE_BITS) }
};

BOOT_CODE int
get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t
get_dev_p_reg(unsigned int i)
{
    return dev_p_regs[i];
}


/* Determine if the given IRQ should be reserved by the kernel. */
bool_t CONST
isReservedIRQ(irq_t irq)
{
    return irq == KERNEL_TIMER_IRQ;
}

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
    printf("Received reserved IRQ: %d\n", (int)irq);
}


BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: timer used as PIT */
    map_kernel_frame(
        TIMER_PADDR,
        TIMER_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
    /* map kernel device: GIC */
    map_kernel_frame(
        GIC_CONTROLLER0_PADDR,
        GIC_CONTROLLER_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
    map_kernel_frame(
        GIC_DISTRIBUTOR_PADDR,
        GIC_DISTRIBUTOR_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

#if defined(DEBUG)
    /* map kernel device: UART */
    map_kernel_frame(
        UART_PADDR,
        UART_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif /* DEBUG */
}

