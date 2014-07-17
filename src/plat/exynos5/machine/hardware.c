/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

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
const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 2 GiB */
    { /* .start = */ 0x40000000, /* .end = */ 0xC0000000 }
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

const p_region_t BOOT_RODATA dev_p_regs[] = {
    { /* .start */ AUDIO_GPIO_PADDR      , /* .end */ AUDIO_GPIO_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ CHIP_ID_PADDR         , /* .end */ CHIP_ID_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_CPU_PADDR         , /* .end */ CMU_CPU_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_CORE_PADDR        , /* .end */ CMU_CORE_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ CMU_ACP_PADDR         , /* .end */ CMU_ACP_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_ISP_PADDR         , /* .end */ CMU_ISP_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_TOP_PADDR         , /* .end */ CMU_TOP_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_LEX_PADDR         , /* .end */ CMU_LEX_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_R0X_PADDR         , /* .end */ CMU_R0X_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_R1X_PADDR         , /* .end */ CMU_R1X_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ CMU_CDREX_PADDR       , /* .end */ CMU_CDREX_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ ALIVE_PADDR           , /* .end */ ALIVE_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SYSREG_PADDR          , /* .end */ SYSREG_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ TMU_PADDR             , /* .end */ TMU_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ MONOTONIC_CNT_PADDR   , /* .end */ MONOTONIC_CNT_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ HDMI_CEC_PADDR        , /* .end */ HDMI_CEC_PADDR         + (1 << PAGE_BITS) },
//  { /* .start */ MCT_PADDR             , /* .end */ MCT_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ WDT_PADDR             , /* .end */ WDT_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ RTC_PADDR             , /* .end */ RTC_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ INT_COMB_CPU_PADDR    , /* .end */ INT_COMB_CPU_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ INT_COMB_IOP_PADDR    , /* .end */ INT_COMB_IOP_PADDR     + (1 << PAGE_BITS) },
//  { /* .start */ GIC_PADDR             , /* .end */ GIC_PADDR              + (8 << PAGE_BITS) },
    { /* .start */ GIC_IOPC_PADDR        , /* .end */ GIC_IOPC_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ GIC_IOPD_PADDR        , /* .end */ GIC_IOPD_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ MPCORE_PRIV_REG_PADDR , /* .end */ MPCORE_PRIV_REG_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ NS_MDMA0_PADDR        , /* .end */ NS_MDMA0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SSS_PADDR             , /* .end */ SSS_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ SSS_KEY_PADDR         , /* .end */ SSS_KEY_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ ENGINE_2D_PADDR       , /* .end */ ENGINE_2D_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ CSSYS_PADDR           , /* .end */ CSSYS_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ A15_EAGLE_PADDR       , /* .end */ A15_EAGLE_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ A5_IOP_PADDR          , /* .end */ A5_IOP_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ A5_ISP_PADDR          , /* .end */ A5_ISP_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_MDMA_PADDR     , /* .end */ SYSMMU_MDMA_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_SSS_PADDR      , /* .end */ SYSMMU_SSS_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_2D_PADDR       , /* .end */ SYSMMU_2D_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ DREXII_PHY0_PADDR     , /* .end */ DREXII_PHY0_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ DREXII_PHY1_PADDR     , /* .end */ DREXII_PHY1_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ AS_A_3D_PADDR         , /* .end */ AS_A_3D_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ AS_A_C2C_PADDR        , /* .end */ AS_A_C2C_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AS_A_LEFT_BUS_PADDR   , /* .end */ AS_A_LEFT_BUS_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ AS_A_RIGHT0_BUS_PADDR , /* .end */ AS_A_RIGHT0_BUS_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ AS_A_DISP1_BUS_PADDR  , /* .end */ AS_A_DISP1_BUS_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ C2C_GPIO_PADDR        , /* .end */ C2C_GPIO_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ DREXII_PADDR          , /* .end */ DREXII_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ AS_A_EFCON_PADDR      , /* .end */ AS_A_EFCON_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ AP_C2C_PADDR          , /* .end */ AP_C2C_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ CP_C2C_PADDR          , /* .end */ CP_C2C_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ AS_A_ACP_BLK_PADDR    , /* .end */ AS_A_ACP_BLK_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ AS_A_CPU_P_BLK_PADDR  , /* .end */ AS_A_CPU_P_BLK_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ AS_A_LBX_BUS_PADDR    , /* .end */ AS_A_LBX_BUS_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ AS_A_R1BX_BUS_PADDR   , /* .end */ AS_A_R1BX_BUS_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ AS_A_R0BX_BUS_PADDR   , /* .end */ AS_A_R0BX_BUS_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ AS_A_CPU_PADDR        , /* .end */ AS_A_CPU_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ MFC_PADDR             , /* .end */ MFC_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_MFC0_PADDR     , /* .end */ SYSMMU_MFC0_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_MFC1_PADDR     , /* .end */ SYSMMU_MFC1_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ GPIO_LEFT_PADDR       , /* .end */ GPIO_LEFT_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ AS_A_MFC_PADDR        , /* .end */ AS_A_MFC_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GENX_PADDR       , /* .end */ AS_A_GENX_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ ENGINE_3D_PADDR       , /* .end */ ENGINE_3D_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ ROTATOR_PADDR         , /* .end */ ROTATOR_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ NS_MDMA1_PADDR        , /* .end */ NS_MDMA1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_ROTATOR_PADDR  , /* .end */ SYSMMU_ROTATOR_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_MDMA1_PADDR    , /* .end */ SYSMMU_MDMA1_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ AS_A_FILE_PADDR       , /* .end */ AS_A_FILE_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GPS_PADDR        , /* .end */ AS_A_GPS_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AS_A_JPEG_PADDR       , /* .end */ AS_A_JPEG_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ JPEG_PADDR            , /* .end */ JPEG_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_JPEG_PADDR     , /* .end */ SYSMMU_JPEG_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ USB3_DEV_LINK_PADDR   , /* .end */ USB3_DEV_LINK_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ USB3_DEV_CTRL_PADDR   , /* .end */ USB3_DEV_CTRL_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ USB2_HOST_EHCI_PADDR  , /* .end */ USB2_HOST_EHCI_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ USB2_HOST_OHCI_PADDR  , /* .end */ USB2_HOST_OHCI_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ USB2_HOST_CTRL_PADDR  , /* .end */ USB2_HOST_CTRL_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ USB2_DEV_LINK_PADDR   , /* .end */ USB2_DEV_LINK_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ MIPI_HSI_PADDR        , /* .end */ MIPI_HSI_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SATA_PHY_CTRL_PADDR   , /* .end */ SATA_PHY_CTRL_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ MCUCTL_IOP_PADDR      , /* .end */ MCUCTL_IOP_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ WDT_IOP_PADDR         , /* .end */ WDT_IOP_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ PDMA0_PADDR           , /* .end */ PDMA0_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ PDMA1_PADDR           , /* .end */ PDMA1_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ RTIC_PADDR            , /* .end */ RTIC_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SATA_I2CPHYCTRL_PADDR , /* .end */ SATA_I2CPHYCTRL_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ MSH0_PADDR            , /* .end */ MSH0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ MSH1_PADDR            , /* .end */ MSH1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ MSH2_PADDR            , /* .end */ MSH2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ MSH3_PADDR            , /* .end */ MSH3_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SROMC_PADDR           , /* .end */ SROMC_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SATA_PADDR            , /* .end */ SATA_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ AXI_FILE_D64_PADDR    , /* .end */ AXI_FILE_D64_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ AXI_FILE_D64_PADDR    , /* .end */ AXI_FILE_D64_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ AXI_USBSATA_D64_PADDR , /* .end */ AXI_USBSATA_D64_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ AXI_USBSATA_D64_PADDR , /* .end */ AXI_USBSATA_D64_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_IOPROC_PADDR   , /* .end */ SYSMMU_IOPROC_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_RTIC_PADDR     , /* .end */ SYSMMU_RTIC_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ AS_A_IOP_FD64X_PADDR  , /* .end */ AS_A_IOP_FD64X_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ AS_A_AUDIO_PADDR      , /* .end */ AS_A_AUDIO_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ AXI_GPS_PADDR         , /* .end */ AXI_GPS_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ AXI_GPS_PADDR         , /* .end */ AXI_GPS_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GPSCPU_PADDR     , /* .end */ AS_A_GPSCPU_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_GPS_PADDR      , /* .end */ SYSMMU_GPS_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ UART0_PADDR           , /* .end */ UART0_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ UART1_PADDR           , /* .end */ UART1_PADDR            + (1 << PAGE_BITS) },

    { /* .start */ UART2_PADDR           , /* .end */ UART2_PADDR            + (1 << PAGE_BITS) },

    { /* .start */ UART3_PADDR           , /* .end */ UART3_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ USI0_PADDR            , /* .end */ USI0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C0_PADDR            , /* .end */ I2C0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C1_PADDR            , /* .end */ I2C1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C2_PADDR            , /* .end */ I2C2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C3_PADDR            , /* .end */ I2C3_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C4_PADDR            , /* .end */ I2C4_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C5_PADDR            , /* .end */ I2C5_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C6_PADDR            , /* .end */ I2C6_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C7_PADDR            , /* .end */ I2C7_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C_HDMI_PADDR        , /* .end */ I2C_HDMI_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ USI1_PADDR            , /* .end */ USI1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ TSADC_PADDR           , /* .end */ TSADC_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SPI0_PADDR            , /* .end */ SPI0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SPI1_PADDR            , /* .end */ SPI1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SPI2_PADDR            , /* .end */ SPI2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ USI2_PADDR            , /* .end */ USI2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2S1_PADDR            , /* .end */ I2S1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2S2_PADDR            , /* .end */ I2S2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ PCM1_PADDR            , /* .end */ PCM1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ PCM2_PADDR            , /* .end */ PCM2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ AC97_PADDR            , /* .end */ AC97_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SPDIF_PADDR           , /* .end */ SPDIF_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ PWM_PADDR             , /* .end */ PWM_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ USI3_PADDR            , /* .end */ USI3_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ FIMC_ISP_PADDR        , /* .end */ FIMC_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ FIMC_DRC_TOP_PADDR    , /* .end */ FIMC_DRC_TOP_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ FIMC_SCALERC_PADDR    , /* .end */ FIMC_SCALERC_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ FIMC_SCALERP_PADDR    , /* .end */ FIMC_SCALERP_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ FIMC_FD_TOP_PADDR     , /* .end */ FIMC_FD_TOP_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ FIMC_ODC_PADDR        , /* .end */ FIMC_ODC_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ FIMC_DIS_PADDR        , /* .end */ FIMC_DIS_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ FIMC_3DNR_PADDR       , /* .end */ FIMC_3DNR_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ ASYNC_AXI_M_PADDR     , /* .end */ ASYNC_AXI_M_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ MPWM_ISP_PADDR        , /* .end */ MPWM_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ I2C2_ISP_PADDR        , /* .end */ I2C2_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ I2C0_ISP_PADDR        , /* .end */ I2C0_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ I2C1_ISP_PADDR        , /* .end */ I2C1_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ MTCADC_ISP_PADDR      , /* .end */ MTCADC_ISP_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ PWM_ISP_PADDR         , /* .end */ PWM_ISP_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ WDT_ISP_PADDR         , /* .end */ WDT_ISP_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ MCUCTL_ISP_PADDR      , /* .end */ MCUCTL_ISP_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ UART_ISP_PADDR        , /* .end */ UART_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SPI0_ISP_PADDR        , /* .end */ SPI0_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SPI1_ISP_PADDR        , /* .end */ SPI1_ISP_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ GIC_C_ISP_PADDR       , /* .end */ GIC_C_ISP_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ GIC_D_ISP_PADDR       , /* .end */ GIC_D_ISP_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCISP_PADDR  , /* .end */ SYSMMU_FIMCISP_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCDRC_PADDR  , /* .end */ SYSMMU_FIMCDRC_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCSCLC_PADDR , /* .end */ SYSMMU_FIMCSCLC_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCSCLP_PADDR , /* .end */ SYSMMU_FIMCSCLP_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCFD_PADDR   , /* .end */ SYSMMU_FIMCFD_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_ISPCPU_PADDR   , /* .end */ SYSMMU_ISPCPU_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCODC_PADDR  , /* .end */ SYSMMU_FIMCODC_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCDIS0_PADDR , /* .end */ SYSMMU_FIMCDIS0_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCDIS1_PADDR , /* .end */ SYSMMU_FIMCDIS1_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMC3DNR_PADDR , /* .end */ SYSMMU_FIMC3DNR_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ GPIO_RIGHT_PADDR      , /* .end */ GPIO_RIGHT_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ AS_A_MFC0_PADDR       , /* .end */ AS_A_MFC0_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ AS_A_ISP0_PADDR       , /* .end */ AS_A_ISP0_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ AS_A_ISP1_PADDR       , /* .end */ AS_A_ISP1_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ AS_A_RIGHT1_PADDR     , /* .end */ AS_A_RIGHT1_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ FIMCLT0_PADDR         , /* .end */ FIMCLT0_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ FIMCLT1_PADDR         , /* .end */ FIMCLT1_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ MIPI_CSI0_PADDR       , /* .end */ MIPI_CSI0_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ MIPI_CSI1_PADDR       , /* .end */ MIPI_CSI1_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCLT0_PADDR  , /* .end */ SYSMMU_FIMCLT0_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCLT1_PADDR  , /* .end */ SYSMMU_FIMCLT1_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ FIMCLT2_PADDR         , /* .end */ FIMCLT2_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_FIMCLT2_PADDR  , /* .end */ SYSMMU_FIMCLT2_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ GSCALER0_PADDR        , /* .end */ GSCALER0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ GSCALER1_PADDR        , /* .end */ GSCALER1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ GSCALER2_PADDR        , /* .end */ GSCALER2_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ GSCALER3_PADDR        , /* .end */ GSCALER3_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GS0_PADDR        , /* .end */ AS_A_GS0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GS1_PADDR        , /* .end */ AS_A_GS1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GS2_PADDR        , /* .end */ AS_A_GS2_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GS3_PADDR        , /* .end */ AS_A_GS3_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_GSCALER0_PADDR , /* .end */ SYSMMU_GSCALER0_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_GSCALER1_PADDR , /* .end */ SYSMMU_GSCALER1_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_GSCALER2_PADDR , /* .end */ SYSMMU_GSCALER2_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_GSCALER3_PADDR , /* .end */ SYSMMU_GSCALER3_PADDR  + (1 << PAGE_BITS) },
    { /* .start */ AS_A_GSCALER_PADDR    , /* .end */ AS_A_GSCALER_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ DISP1_MIX_PADDR       , /* .end */ DISP1_MIX_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ DISP1_ENH_PADDR       , /* .end */ DISP1_ENH_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ DISP1_CTRL_PADDR      , /* .end */ DISP1_CTRL_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ MIE_PADDR             , /* .end */ MIE_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ TV_MIXER_PADDR        , /* .end */ TV_MIXER_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ MIPI_DSI1_PADDR       , /* .end */ MIPI_DSI1_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ DP1_PADDR             , /* .end */ DP1_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ HDMI_0_PADDR          , /* .end */ HDMI_0_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ HDMI_1_PADDR          , /* .end */ HDMI_1_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ HDMI_2_PADDR          , /* .end */ HDMI_2_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ HDMI_3_PADDR          , /* .end */ HDMI_3_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ HDMI_4_PADDR          , /* .end */ HDMI_4_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ HDMI_5_PADDR          , /* .end */ HDMI_5_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ HDMI_6_PADDR          , /* .end */ HDMI_6_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ DP1_1_PADDR           , /* .end */ DP1_1_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_DISP1_PADDR    , /* .end */ SYSMMU_DISP1_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ SYSMMU_TV_PADDR       , /* .end */ SYSMMU_TV_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ AS_A_TV_PADDR         , /* .end */ AS_A_TV_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ AES0EF0_PADDR         , /* .end */ AES0EF0_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ EFCON0_SFR_PADDR      , /* .end */ EFCON0_SFR_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ AES0_SFR_PADDR        , /* .end */ AES0_SFR_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AES1EF1_PADDR         , /* .end */ AES1EF1_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ EFCON1_SFR_PADDR      , /* .end */ EFCON1_SFR_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ NS_NDMA_PADDR         , /* .end */ NS_NDMA_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ S_NDMA_PADDR          , /* .end */ S_NDMA_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ AES1_SFR_PADDR        , /* .end */ AES1_SFR_PADDR         + (1 << PAGE_BITS) },
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
    return irq == INTERRUPT_WDT;
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
    /* map kernel device: Watch dog timer used as PIT */
    map_kernel_frame(
        MCT_PADDR,
        MCT_PPTR,
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
        UART2_PADDR,
        UART_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif /* DEBUG */
}

