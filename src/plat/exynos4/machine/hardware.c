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
    /* 1 GiB */
    { /* .start = */ 0x40000000, /* .end = */ 0x80000000 }
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
    { /* .start */ CHIPID_PADDR           , /* .end */ CHIPID_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SYSREG_PADDR           , /* .end */ SYSREG_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ PMU_PADDR              , /* .end */ PMU_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ CMU_TOP_PART_PADDR     , /* .end */ CMU_TOP_PART_PADDR      + (13 << PAGE_BITS) },
    { /* .start */ SMDMA0_PADDR           , /* .end */ SMDMA0_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ CMU_DMC_PART_PADDR     , /* .end */ CMU_DMC_PART_PADDR      + (9 << PAGE_BITS) },
    { /* .start */ NSMDMA0_PADDR          , /* .end */ NSMDMA0_PADDR           + (1 << PAGE_BITS) },
    /* NOTE: exynos4 does not have the standard A9 private timer, this is Samsungs MCT private timer */
//  { /* .start */ MCT_PADDR              , /* .end */ MCT_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ SSS_PADDR              , /* .end */ SSS_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ WDT_PADDR              , /* .end */ WDT_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ CORESIGHT_PADDR        , /* .end */ CORESIGHT_PADDR         + (2 << PAGE_BITS) },
    { /* .start */ RTC_PADDR              , /* .end */ RTC_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ KEYIF_PADDR            , /* .end */ KEYIF_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ HDMI_CEC_PADDR         , /* .end */ HDMI_CEC_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ TMU_PADDR              , /* .end */ TMU_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ AXI_ACPX_PADDR         , /* .end */ AXI_ACPX_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ SECKEY_PADDR           , /* .end */ SECKEY_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SMMUMDMA_PADDR         , /* .end */ SMMUMDMA_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ TZPC0_PADDR            , /* .end */ TZPC0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SMMUSSS_PADDR          , /* .end */ SMMUSSS_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ TZPC1_PADDR            , /* .end */ TZPC1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ QEMDMA_PADDR           , /* .end */ QEMDMA_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ TZPC2_PADDR            , /* .end */ TZPC2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ QESSS_PADDR            , /* .end */ QESSS_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ TZPC3_PADDR            , /* .end */ TZPC3_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ TZPC4_PADDR            , /* .end */ TZPC4_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ TZPC5_PADDR            , /* .end */ TZPC5_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ DMC0_PADDR             , /* .end */ DMC0_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ AXI_MAUDIOX_PADDR      , /* .end */ AXI_MAUDIOX_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ DMC1_PADDR             , /* .end */ DMC1_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ INT_COMBINER_PADDR     , /* .end */ INT_COMBINER_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ GPIO_RIGHT_PADDR       , /* .end */ GPIO_RIGHT_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ IEM_IEC_PADDR          , /* .end */ IEM_IEC_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ AXI_GDR_PADDR          , /* .end */ AXI_GDR_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ IEM_APC_PADDR          , /* .end */ IEM_APC_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ AXI_GPR_PADDR          , /* .end */ AXI_GPR_PADDR           + (1 << PAGE_BITS) },
//  { /* .start */ GIC_CONTROLLER0_PADDR  , /* .end */ GIC_CONTROLLER0_PADDR   + (1 << PAGE_BITS) },
//  { /* .start */ GIC_CONTROLLER1_PADDR  , /* .end */ GIC_CONTROLLER1_PADDR   + (1 << PAGE_BITS) },
//  { /* .start */ GIC_DISTRIBUTOR_PADDR  , /* .end */ GIC_DISTRIBUTOR_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_CAMIF_PADDR   , /* .end */ ASYNCAXI_CAMIF_PADDR    + (1 << PAGE_BITS) },
//  { /* .start */ MPCORE_PRIV_REG_PADDR  , /* .end */ MPCORE_PRIV_REG_PADDR   + (2 << PAGE_BITS) },
    { /* .start */ L2CC_PADDR             , /* .end */ L2CC_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_LCD0_PADDR    , /* .end */ ASYNCAXI_LCD0_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_LCD1_PADDR    , /* .end */ ASYNCAXI_LCD1_PADDR     + (1 << PAGE_BITS) },
    { /* .start */ AXI_DMCD_PADDR         , /* .end */ AXI_DMCD_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_FSYSD_PADDR   , /* .end */ ASYNCAXI_FSYSD_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ AXI_DMCSFRX_PADDR      , /* .end */ AXI_DMCSFRX_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_MAUDIO_PADDR  , /* .end */ ASYNCAXI_MAUDIO_PADDR   + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_GDL_DMCD_PADDR, /* .end */ ASYNCAXI_GDL_DMCD_PADDR + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_GDR_DMCD_PADDR, /* .end */ ASYNCAXI_GDR_DMCD_PADDR + (1 << PAGE_BITS) },
    { /* .start */ QECPU_PADDR            , /* .end */ QECPU_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ GPIO_LEFT_PADDR        , /* .end */ GPIO_LEFT_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AXI_GDL_PADDR          , /* .end */ AXI_GDL_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ AXI_GPL_PADDR          , /* .end */ AXI_GPL_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ AXI_LCD0_PADDR         , /* .end */ AXI_LCD0_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_IMAGE_PADDR   , /* .end */ ASYNCAXI_IMAGE_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ SMMUFIMD0_PADDR        , /* .end */ SMMUFIMD0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_TV_PADDR      , /* .end */ ASYNCAXI_TV_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ ASYNCAXI_MFC_L_PADDR   , /* .end */ ASYNCAXI_MFC_L_PADDR    + (1 << PAGE_BITS) },
    { /* .start */ FIMD1_PADDR            , /* .end */ FIMD1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ FIMC0_PADDR            , /* .end */ FIMC0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ FIMC1_PADDR            , /* .end */ FIMC1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ MIE1_PADDR             , /* .end */ MIE1_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ FIMC2_PADDR            , /* .end */ FIMC2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ FIMC3_PADDR            , /* .end */ FIMC3_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ JPEG_PADDR             , /* .end */ JPEG_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ MIPI_DSI1_PADDR        , /* .end */ MIPI_DSI1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ MIPI_CSI0_PADDR        , /* .end */ MIPI_CSI0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ MIPI_CSI1_PADDR        , /* .end */ MIPI_CSI1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AXI_CAMX_PADDR         , /* .end */ AXI_CAMX_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ AXI_LCD1X_PADDR        , /* .end */ AXI_LCD1X_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMUFIMC0_PADDR        , /* .end */ SMMUFIMC0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMUFIMD1_PADDR        , /* .end */ SMMUFIMD1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMUFIMC1_PADDR        , /* .end */ SMMUFIMC1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMUFIMC2_PADDR        , /* .end */ SMMUFIMC2_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ PPMU_LCD1_PADDR        , /* .end */ PPMU_LCD1_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMUFIMC3_PADDR        , /* .end */ SMMUFIMC3_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMUJPEG_PADDR         , /* .end */ SMMUJPEG_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ PCIE_PADDR             , /* .end */ PCIE_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ QEFIMC0_PADDR          , /* .end */ QEFIMC0_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ QEFIMC1_PADDR          , /* .end */ QEFIMC1_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ USBDEV_LINK_PADDR      , /* .end */ USBDEV_LINK_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ QEFIMC2_PADDR          , /* .end */ QEFIMC2_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ QEFIMC3_PADDR          , /* .end */ QEFIMC3_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ FIMD0_PADDR            , /* .end */ FIMD0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ TSI_PADDR              , /* .end */ TSI_PADDR               + (1 << PAGE_BITS) },
    { /* .start */ SDMMC0_PADDR           , /* .end */ SDMMC0_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ MIE0_PADDR             , /* .end */ MIE0_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ SDMMC1_PADDR           , /* .end */ SDMMC1_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SDMMC2_PADDR           , /* .end */ SDMMC2_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SDMMC3_PADDR           , /* .end */ SDMMC3_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ MIPI_DSI0_PADDR        , /* .end */ MIPI_DSI0_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SDMMC4_PADDR           , /* .end */ SDMMC4_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SATA_PADDR             , /* .end */ SATA_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ SMMUMDMA2_PADDR        , /* .end */ SMMUMDMA2_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SROMC_PADDR            , /* .end */ SROMC_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ USBHOST_EHCI_PADDR     , /* .end */ USBHOST_EHCI_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ QEG2D_PADDR            , /* .end */ QEG2D_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ USBHOST_OHCI_PADDR     , /* .end */ USBHOST_OHCI_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ QEROTATOR_PADDR        , /* .end */ QEROTATOR_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ QEMDMA2_PADDR          , /* .end */ QEMDMA2_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ USBPHY_CON_PADDR       , /* .end */ USBPHY_CON_PADDR        + (1 << PAGE_BITS) },
    { /* .start */ PCIEPHY_PADDR          , /* .end */ PCIEPHY_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SATA_PHY_PADDR         , /* .end */ SATA_PHY_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ VP_PADDR               , /* .end */ VP_PADDR                + (1 << PAGE_BITS) },
    { /* .start */ MIXER_PADDR            , /* .end */ MIXER_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ AXI_FSYSD_PADDR        , /* .end */ AXI_FSYSD_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ TVENC_PADDR            , /* .end */ TVENC_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ AXI_FSYSS_PADDR        , /* .end */ AXI_FSYSS_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ SMMUPCIE_PADDR         , /* .end */ SMMUPCIE_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ HDMI0_PADDR            , /* .end */ HDMI0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ HDMI1_PADDR            , /* .end */ HDMI1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ HDMI2_PADDR            , /* .end */ HDMI2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ PDMA0_PADDR            , /* .end */ PDMA0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ HDMI3_PADDR            , /* .end */ HDMI3_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ PDMA1_PADDR            , /* .end */ PDMA1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ HDMI4_PADDR            , /* .end */ HDMI4_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ PCIEPHY_CTRL_PADDR     , /* .end */ PCIEPHY_CTRL_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ HDMI5_PADDR            , /* .end */ HDMI5_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SATAPHY_CTRL_PADDR     , /* .end */ SATAPHY_CTRL_PADDR      + (1 << PAGE_BITS) },
    { /* .start */ HDMI6_PADDR            , /* .end */ HDMI6_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ AXI_TVX_PADDR          , /* .end */ AXI_TVX_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SMMUTV_PADDR           , /* .end */ SMMUTV_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SMDMA2_PADDR           , /* .end */ SMDMA2_PADDR            + (1 << PAGE_BITS) },
    { /* .start */ SMMUMFC_L_PADDR        , /* .end */ SMMUMFC_L_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ NSMDMA2_PADDR          , /* .end */ NSMDMA2_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ SMMUMFC_R_PADDR        , /* .end */ SMMUMFC_R_PADDR         + (1 << PAGE_BITS) },
    { /* .start */ AXI_IMGX_PADDR         , /* .end */ AXI_IMGX_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ SMMUG2D_PADDR          , /* .end */ SMMUG2D_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ UART0_PADDR            , /* .end */ UART0_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SMMUROTATOR_PADDR      , /* .end */ SMMUROTATOR_PADDR       + (1 << PAGE_BITS) },
    { /* .start */ UART1_PADDR            , /* .end */ UART1_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ UART2_PADDR            , /* .end */ UART2_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ UART3_PADDR            , /* .end */ UART3_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ UART4_PADDR            , /* .end */ UART4_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ I2C0_PADDR             , /* .end */ I2C0_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2C1_PADDR             , /* .end */ I2C1_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2C2_PADDR             , /* .end */ I2C2_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2C3_PADDR             , /* .end */ I2C3_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2C4_PADDR             , /* .end */ I2C4_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2C5_PADDR             , /* .end */ I2C5_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2C6_PADDR             , /* .end */ I2C6_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2C7_PADDR             , /* .end */ I2C7_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2CHDMI_PADDR          , /* .end */ I2CHDMI_PADDR           + (1 << PAGE_BITS) },
    { /* .start */ TSADC_PADDR            , /* .end */ TSADC_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ SPI0_PADDR             , /* .end */ SPI0_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ SPI1_PADDR             , /* .end */ SPI1_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ SPI2_PADDR             , /* .end */ SPI2_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2S1_PADDR             , /* .end */ I2S1_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ I2S2_PADDR             , /* .end */ I2S2_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ PCM1_PADDR             , /* .end */ PCM1_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ PCM2_PADDR             , /* .end */ PCM2_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ AC97_PADDR             , /* .end */ AC97_PADDR              + (1 << PAGE_BITS) },
    { /* .start */ SPDIF_PADDR            , /* .end */ SPDIF_PADDR             + (1 << PAGE_BITS) },
    { /* .start */ PWMTIMER_PADDR         , /* .end */ PWMTIMER_PADDR          + (1 << PAGE_BITS) },
    { /* .start */ MODEMIF_PADDR          , /* .end */ MODEMIF_PADDR           + (1 << PAGE_BITS) },
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

    /* map kernel device: L2CC */
    map_kernel_frame(
        L2CC_PADDR,
        L2CC_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

#ifdef DEBUG
    /* map kernel device: UART */
    map_kernel_frame(
        UART1_PADDR,
        UART1_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif /* DEBUG */
}

