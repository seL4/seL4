/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#include <config.h>
#include <util.h>
#include <basic_types.h>
#include <linker.h>
#include <arch/object/vcpu.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware_gen.h>
#include <plat/machine/smmu.h>
#include <mode/machine/hardware.h>
#include <mode/api/constants.h>
#include <arch/benchmark_overflowHandler.h>

#define physBase          0x80000000
#define kernelBase        0xe0000000

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /* map kernel device: GIC */
        GIC_CONTROLLER0_PADDR,
        GIC_CONTROLLER_PPTR,
        true  /* armExecuteNever */
    },
    {
        GIC_DISTRIBUTOR_PADDR,
        GIC_DISTRIBUTOR_PPTR,
        true  /* armExecuteNever */

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    },
    {
        GIC_VCPUCTRL_PADDR,
        GIC_VCPUCTRL_PPTR,
        false
#endif /* CONFIG_ARM_HYPERVISOR */
#ifdef CONFIG_ARM_SMMU
    },
    {
        MC_PADDR,
        SMMU_PPTR,
        false
#endif /* CONFIG_ARM_SMMU */
#ifdef CONFIG_PRINTING
    },
    {
        /* UART */
        UARTA_PADDR,
        UARTA_PPTR,
        true
#endif /* CONFIG_PRINTING */
    }
};
/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

/* 1 MiB starting from 0xa7f00000 is reserved by the elfloader for monitor mode hooks */
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    { .start = 0x80000000, .end = 0xa7f00000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    { TK1_EXTRA_RAM_START,  TK1_EXTRA_RAM_START + TK1_EXTRA_RAM_SIZE },
    { VGICI_VM_PADDR,       VGICI_VM_PADDR + BIT(seL4_PageBits) },
    { PCIE_0_CFG_PADDR,     PCIE_0_CFG_PADDR + BIT(seL4_PageBits) },
    { PCIE_1_CFG_PADDR,     PCIE_1_CFG_PADDR + BIT(seL4_PageBits) },
    { PCIE_PCA0_1_PADDR,    PCIE_PCA0_1_PADDR + BIT(seL4_PageBits) },
    { PCIE_PADS_AFI_PADDR,  PCIE_PADS_AFI_PADDR + BIT(seL4_PageBits) },
    { PCIE_A2_PADDR,        PCIE_A2_PADDR + BIT(20) * 224 },
    { R8169_NIC_PADDR,      R8169_NIC_PADDR + BIT(20)},
    { GRAPH_HOST_REGS_PADDR, GRAPH_HOST_REGS_PADDR + BIT(10) * 208},           /* 208KB */
    { GRAPH_HOST_PADDR,     GRAPH_HOST_PADDR + (BIT(20) * 16) },               /* 16 MB                        */
    { GPU_PADDR,            GPU_PADDR + (BIT(20) * 144) },                     /* 144 MB                       */
    { UP_TAG_PADDR,         UP_TAG_PADDR + BIT(seL4_PageBits) },               /* 4 KB                         */
    { RSEM_PADDR,           RSEM_PADDR + BIT(seL4_PageBits) },                 /* 4 KB                         */
    { ASEM_PADDR,           ASEM_PADDR + BIT(seL4_PageBits) },                 /* 4 KB                         */
    { ARB_PRI_PADDR,        ARB_PRI_PADDR + BIT(seL4_PageBits) },              /* 4 KB                         */
    { ICTLR_PADDR,          ICTLR_PADDR + BIT(seL4_PageBits) },                /* 4 KB, includes several       */
    { TMR_PADDR,            TMR_PADDR + BIT(seL4_PageBits) },                  /* 4 Kb,  1 KB                  */
    { CLK_RESET_PADDR,      CLK_RESET_PADDR + BIT(seL4_PageBits) },            /* 4 KB                         */
    { FLOW_CTRL_PADDR,      FLOW_CTRL_PADDR + BIT(seL4_PageBits) },            /* 4 KB                         */
    { AHB_DMA_PADDR,        AHB_DMA_PADDR + (BIT(seL4_PageBits) * 2) },        /* 8 KB                         */
    { AHB_DMA_CH_PADDR,     AHB_DMA_CH_PADDR + BIT(seL4_PageBits) },           /* 4 KB 4 channels, 32 bytes    */
    { APB_DMA_PADDR,        APB_DMA_PADDR + (BIT(seL4_PageBits) * 4) },        /* 16 KB                        */
    { APB_DMA_CH_PADDR,     APB_DMA_CH_PADDR + BIT(seL4_PageBits) },           /* 4KB 32 channels, 64 bytes    */
    { SYS_REGS_PADDR,       SYS_REGS_PADDR + BIT(seL4_PageBits) },             /* 768 bytes + 2 KB             */
    { GPIO_PADDR,           GPIO_PADDR + BIT(seL4_PageBits) },                 /* 8 GPIOs, 265 bytes each      */
    { VCP_PADDR,            VCP_PADDR + BIT(seL4_PageBits) },                  /* 4 KB                         */
    { VPUCQ_PADDR,          VPUCQ_PADDR + BIT(seL4_PageBits) },                /* 256 Bytes                    */
    { BSEA_PADDR,           BSEA_PADDR + BIT(seL4_PageBits) },                 /* 4 KB                         */
    { IPATCH_PADDR,         IPATCH_PADDR + BIT(seL4_PageBits) },               /* 4 KB offset 0xc00, 1 KB      */
    { VDE_FRAMEID_PADDR,    VDE_FRAMEID_PADDR + (BIT(seL4_PageBits) * 4) },    /* 16 KB, multiple              */
    { MISC_AUX_PADDR,       MISC_AUX_PADDR + (BIT(seL4_PageBits) * 3) },       /* 12 KB                        */
    { MISC_PINMUX_PADDR,    MISC_PINMUX_PADDR + BIT(seL4_PageBits) },          /* 4 KB                         */
    { UARTA_SYNC_PADDR,     UARTA_SYNC_PADDR + (BIT(seL4_PageBits) * 3 ) },    /* 12 KB, multiple              */
    { SYNC_NOR_PADDR,       SYNC_NOR_PADDR + BIT(seL4_PageBits) },             /* 4 KB                         */
    { PWM_PADDR,            PWM_PADDR + BIT(seL4_PageBits) },                  /* 4 KB, 256 bytes              */
    { MIPIHSI_PADDR,        MIPIHSI_PADDR + BIT(seL4_PageBits) },              /* 4 KB                         */
    { I2C_I2C4_PADDR,       I2C_I2C4_PADDR + BIT(seL4_PageBits) },             /* 4 KB                         */
    { I2C5_SPI2B_6_PADDR,   I2C5_SPI2B_6_PADDR + BIT(seL4_PageBits) },         /* 4 KB                         */
    { RTC_KFUSE_PADDR,      RTC_KFUSE_PADDR + BIT(seL4_PageBits) },            /* 4 KB                         */
    { FUSE_KFUSE_PADDR,     FUSE_KFUSE_PADDR + BIT(seL4_PageBits) },            /* 4 KB                         */
    { LA_PADDR,             LA_PADDR + (BIT(seL4_PageBits) * 2) },             /* 8 KB                         */
    { SE_PADDR,             SE_PADDR + (BIT(seL4_PageBits) * 2) },             /* 8 KB                         */
    { TSENSOR_PADDR,        TSENSOR_PADDR + BIT(seL4_PageBits) },              /* 4 KB                         */
    { CEC_PADDR,            CEC_PADDR + BIT(seL4_PageBits) },                  /* 4 KB                         */
    { ATOMICS_PADDR,        ATOMICS_PADDR + (BIT(seL4_PageBits) * 2) },        /* 8 KB                         */
#ifndef CONFIG_ARM_SMMU
    { MC_PADDR,             MC_PADDR + BIT(seL4_PageBits) },                   /* 4 KB                         */
#endif
    { EMC_PADDR,            EMC_PADDR + BIT(seL4_PageBits) },                  /* 4 KB                         */
    { SATA_PADDR,           SATA_PADDR + (BIT(seL4_PageBits) * 16) },          /* 64 KB                        */
    { HDA_PADDR,            HDA_PADDR + (BIT(seL4_PageBits) * 16) },           /* 64 KB                        */
    { MIOBFM_PADDR,         MIOBFM_PADDR + (BIT(seL4_PageBits) * 16) },        /* 64 KB                        */
    { AUDIO_PADDR,          AUDIO_PADDR + (BIT(seL4_PageBits) * 16) },         /* 64 KB                        */
    { XUSB_HOST_PADDR,      XUSB_HOST_PADDR + (BIT(seL4_PageBits) * 9) },      /* 36 KB                        */
    { XUSB_PADCTL_PADDR,    XUSB_PADCTL_PADDR + BIT(seL4_PageBits) },          /* 4  KB                        */
    { XUSB_DEV_PADDR,       XUSB_DEV_PADDR + (BIT(seL4_PageBits) * 10) },      /* 40 KB                        */
    { DDS_PADDR,            DDS_PADDR + (BIT(seL4_PageBits) * 2) },            /* 8KB 4608 bytes               */
    { SDMMC_1_PADDR,        SDMMC_1_PADDR + BIT(seL4_PageBits) },              /* 4KB 512 bytes                */
    { SDMMC_1B_4_PADDR,     SDMMC_1B_4_PADDR + (BIT(seL4_PageBits) * 15) },    /* 60KB 512 bytes    each       */
    { SPEEDO_PADDR,         SPEEDO_PADDR + (BIT(seL4_PageBits) * 8) },         /* 32 KB                        */
    { SPEEDO_PMON_PADDR,    SPEEDO_PMON_PADDR + (BIT(seL4_PageBits) * 8) },    /* 32 KB                        */
    { SYSCTR0_PADDR,        SYSCTR0_PADDR + (BIT(seL4_PageBits) * 16) },       /* 64 KB                        */
    { SYSCTR1_PADDR,        SYSCTR1_PADDR + (BIT(seL4_PageBits) * 16) },       /* 64 KB                        */
    { DP2_PADDR,            DP2_PADDR + BIT(seL4_PageBits) },                  /* 4 KB 256 Bytes               */
    { APB2JTAG_PADDR,       APB2JTAG_PADDR + BIT(seL4_PageBits) },             /* 4 Kb 512 Bytes               */
    { SOC_THERM_PADDR,      SOC_THERM_PADDR + BIT(seL4_PageBits) },            /* 4 KB                         */
    { MIPI_CAL_PADDR,       MIPI_CAL_PADDR + BIT(seL4_PageBits) },             /* 4 KB 265 Bytes               */
    { DVFS_PADDR,           DVFS_PADDR + BIT(seL4_PageBits) },                 /* 4 KB 1 KB                    */
    { CLUSTER_CLK_PADDR,    CLUSTER_CLK_PADDR + (BIT(seL4_PageBits) * 64) },   /* 256 KB                       */
    { CSITE_PADDR,          CSITE_PADDR + (BIT(20) * 2) },                     /* 2 MB                         */
    { PPCS_PADDR,           PPCS_PADDR + (BIT(seL4_PageBits) * 16) },          /* 64 KB                        */
    { TZRAM_PADDR,          TZRAM_PADDR + (BIT(seL4_PageBits) * 16) },         /* 64 KB                        */
    { USB_PADDR,            USB_PADDR + (BIT(seL4_PageBits) * 2) },            /* 8 KB region, 6 KB            */
    { USB2_PADDR,           USB2_PADDR + (BIT(seL4_PageBits) * 2) },           /* 8 KB region, 6 KB            */
    { USB3_PADDR,           USB3_PADDR + (BIT(seL4_PageBits) * 2) },           /* 8 KB region, 6 KB            */
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    if (irq == KERNEL_PMU_IRQ) {
        handleOverflowIRQ();
    }
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

    if ((config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) && (irq == INTERRUPT_VGIC_MAINTENANCE)) {
        VGICMaintenance();
        return;
    }

    if (config_set(CONFIG_ARM_SMMU) && (irq == INTERRUPT_SMMU)) {
        plat_smmu_handle_interrupt();
        return;
    }
}

#endif
