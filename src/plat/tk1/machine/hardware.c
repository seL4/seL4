/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>
#include <plat/machine/smmu.h>
#include <arch/benchmark_overflowHandler.h>

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

/* 1 MiB starting from 0xa7f00000 is reserved by the elfloader for monitor mode hooks */
const p_region_t BOOT_RODATA avail_p_regs[] = {
    { .start = 0x80000000, .end = 0xa7f00000 }
};

BOOT_CODE int get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_avail_p_reg(word_t i)
{
    return avail_p_regs[i];
}

#define SECTION_BITS    20
#define PAGE_SIZE       (1 << PAGE_BITS)
#define SECTION_SIZE    (1 << SECTION_BITS)

const p_region_t BOOT_RODATA dev_p_regs[] = {

    { VM_HOST_PA_START,     VM_HOST_PA_START + VM_HOST_PA_SIZE },
    { VGICI_VM_PADDR,       VGICI_VM_PADDR + PAGE_SIZE },
    { PCIE_0_CFG_PADDR,     PCIE_0_CFG_PADDR + PAGE_SIZE },
    { PCIE_1_CFG_PADDR,     PCIE_1_CFG_PADDR + PAGE_SIZE },
    { PCIE_PCA0_1_PADDR,    PCIE_PCA0_1_PADDR + PAGE_SIZE },
    { PCIE_PADS_AFI_PADDR,  PCIE_PADS_AFI_PADDR + PAGE_SIZE },
    { PCIE_A2_PADDR,        PCIE_A2_PADDR + SECTION_SIZE },
    { R8169_NIC_PADDR,      R8169_NIC_PADDR + SECTION_SIZE},
    { GRAPH_HOST_PADDR,     GRAPH_HOST_PADDR + (SECTION_SIZE * 16) }, /* 16 MB                        */
    { GPU_PADDR,            GPU_PADDR + (SECTION_SIZE * 144) },       /* 144 MB                       */
    { UP_TAG_PADDR,         UP_TAG_PADDR + PAGE_SIZE },               /* 4 KB                         */
    { RSEM_PADDR,           RSEM_PADDR + PAGE_SIZE },                 /* 4 KB                         */
    { ASEM_PADDR,           ASEM_PADDR + PAGE_SIZE },                 /* 4 KB                         */
    { ARB_PRI_PADDR,        ARB_PRI_PADDR + PAGE_SIZE },              /* 4 KB                         */
    { ICTLR_PADDR,          ICTLR_PADDR + PAGE_SIZE },                /* 4 KB, includes several       */
    { TMR_PADDR,            TMR_PADDR + PAGE_SIZE },                  /* 4 Kb,  1 KB                  */
    { CLK_RESET_PADDR,      CLK_RESET_PADDR + PAGE_SIZE },            /* 4 KB                         */
    { FLOW_CTRL_PADDR,      FLOW_CTRL_PADDR + PAGE_SIZE },            /* 4 KB                         */
    { AHB_DMA_PADDR,        AHB_DMA_PADDR + (PAGE_SIZE * 2) },        /* 8 KB                         */
    { AHB_DMA_CH_PADDR,     AHB_DMA_CH_PADDR + PAGE_SIZE },           /* 4 KB 4 channels, 32 bytes    */
    { APB_DMA_PADDR,        APB_DMA_PADDR + (PAGE_SIZE * 4) },        /* 16 KB                        */
    { APB_DMA_CH_PADDR,     APB_DMA_CH_PADDR + PAGE_SIZE },           /* 4KB 32 channels, 64 bytes    */
    { SYS_REGS_PADDR,       SYS_REGS_PADDR + PAGE_SIZE },             /* 768 bytes + 2 KB             */
    { GPIO_PADDR,           GPIO_PADDR + PAGE_SIZE },                 /* 8 GPIOs, 265 bytes each      */
    { VCP_PADDR,            VCP_PADDR + PAGE_SIZE },                  /* 4 KB                         */
    { VPUCQ_PADDR,          VPUCQ_PADDR + PAGE_SIZE },                /* 256 Bytes                    */
    { BSEA_PADDR,           BSEA_PADDR + PAGE_SIZE },                 /* 4 KB                         */
    { IPATCH_PADDR,         IPATCH_PADDR + PAGE_SIZE },               /* 4 KB offset 0xc00, 1 KB      */
    { VDE_FRAMEID_PADDR,    VDE_FRAMEID_PADDR + (PAGE_SIZE * 4) },    /* 16 KB, multiple              */
    { MISC_PINMUX_PADDR,    MISC_PINMUX_PADDR + (PAGE_SIZE * 4) },    /* 16 KB                        */
    { UARTA_SYNC_PADDR,     UARTA_SYNC_PADDR + (PAGE_SIZE * 3 ) },    /* 12 KB, multiple              */
    { SYNC_NOR_PADDR,       SYNC_NOR_PADDR + PAGE_SIZE },             /* 4 KB                         */
    { PWM_PADDR,            PWM_PADDR + PAGE_SIZE },                  /* 4 KB, 256 bytes              */
    { MIPIHSI_PADDR,        MIPIHSI_PADDR + PAGE_SIZE },              /* 4 KB                         */
    { I2C_I2C4_PADDR,       I2C_I2C4_PADDR + PAGE_SIZE },             /* 4 KB                         */
    { I2C5_SPI2B_6_PADDR,   I2C5_SPI2B_6_PADDR + PAGE_SIZE },         /* 4 KB                         */
    { RTC_KFUSE_PADDR,      RTC_KFUSE_PADDR + PAGE_SIZE },            /* 4 KB                         */
    { LA_PADDR,             LA_PADDR + (PAGE_SIZE * 2) },             /* 8 KB                         */
    { SE_PADDR,             SE_PADDR + (PAGE_SIZE * 2) },             /* 8 KB                         */
    { TSENSOR_PADDR,        TSENSOR_PADDR + PAGE_SIZE },              /* 4 KB                         */
    { CEC_PADDR,            CEC_PADDR + PAGE_SIZE },                  /* 4 KB                         */
    { ATOMICS_PADDR,        ATOMICS_PADDR + (PAGE_SIZE * 2) },        /* 8 KB                         */
#ifndef CONFIG_ARM_SMMU
    { MC_PADDR,             MC_PADDR + PAGE_SIZE },                   /* 4 KB                         */
#endif
    { EMC_PADDR,            EMC_PADDR + PAGE_SIZE },                  /* 4 KB                         */
    { SATA_PADDR,           SATA_PADDR + (PAGE_SIZE * 16) },          /* 64 KB                        */
    { HDA_PADDR,            HDA_PADDR + (PAGE_SIZE * 16) },           /* 64 KB                        */
    { MIOBFM_PADDR,         MIOBFM_PADDR + (PAGE_SIZE * 16) },        /* 64 KB                        */
    { AUDIO_PADDR,          AUDIO_PADDR + (PAGE_SIZE * 16) },         /* 64 KB                        */
    { XUSB_HOST_PADDR,      XUSB_HOST_PADDR + (PAGE_SIZE * 9) },      /* 36 KB                        */
    { XUSB_PADCTL_PADDR,    XUSB_PADCTL_PADDR + PAGE_SIZE },          /* 4  KB                        */
    { XUSB_DEV_PADDR,       XUSB_DEV_PADDR + (PAGE_SIZE * 10) },      /* 40 KB                        */
    { DDS_PADDR,            DDS_PADDR + (PAGE_SIZE * 2) },            /* 8KB 4608 bytes               */
    { SDMMC_1_PADDR,        SDMMC_1_PADDR + PAGE_SIZE },              /* 4KB 512 bytes                */
    { SDMMC_1B_4_PADDR,     SDMMC_1B_4_PADDR + (PAGE_SIZE * 15) },    /* 60KB 512 bytes    each       */
    { SPEEDO_PADDR,         SPEEDO_PADDR + (PAGE_SIZE * 8) },         /* 32 KB                        */
    { SPEEDO_PMON_PADDR,    SPEEDO_PMON_PADDR + (PAGE_SIZE * 8) },    /* 32 KB                        */
    { SYSCTR0_PADDR,        SYSCTR0_PADDR + (PAGE_SIZE * 16) },       /* 64 KB                        */
    { SYSCTR1_PADDR,        SYSCTR1_PADDR + (PAGE_SIZE * 16) },       /* 64 KB                        */
    { DP2_PADDR,            DP2_PADDR + PAGE_SIZE },                  /* 4 KB 256 Bytes               */
    { APB2JTAG_PADDR,       APB2JTAG_PADDR + PAGE_SIZE },             /* 4 Kb 512 Bytes               */
    { SOC_THERM_PADDR,      SOC_THERM_PADDR + PAGE_SIZE },            /* 4 KB                         */
    { MIPI_CAL_PADDR,       MIPI_CAL_PADDR + PAGE_SIZE },             /* 4 KB 265 Bytes               */
    { DVFS_PADDR,           DVFS_PADDR + PAGE_SIZE },                 /* 4 KB 1 KB                    */
    { CLUSTER_CLK_PADDR,    CLUSTER_CLK_PADDR + (PAGE_SIZE * 64) },   /* 256 KB                       */
    { CSITE_PADDR,          CSITE_PADDR + (SECTION_SIZE * 2) },       /* 2 MB                         */
    { PPCS_PADDR,           PPCS_PADDR + (PAGE_SIZE * 16) },          /* 64 KB                        */
    { TZRAM_PADDR,          TZRAM_PADDR + (PAGE_SIZE * 16) },         /* 64 KB                        */
    { USB_PADDR,            USB_PADDR + (PAGE_SIZE * 2) },            /* 8 KB region, 6 KB            */
    { USB2_PADDR,           USB2_PADDR + (PAGE_SIZE * 2) },           /* 8 KB region, 6 KB            */
    { USB3_PADDR,           USB3_PADDR + (PAGE_SIZE * 2) },           /* 8 KB region, 6 KB            */
};

BOOT_CODE int get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_dev_p_reg(word_t i)
{
    return dev_p_regs[i];
}


/* Handle a platform-reserved IRQ. */
void
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

BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: GIC */
    map_kernel_frame(
        GIC_CONTROLLER0_PADDR,
        GIC_CONTROLLER_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
    map_kernel_frame(
        GIC_DISTRIBUTOR_PADDR,
        GIC_DISTRIBUTOR_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        map_kernel_frame(
            GIC_VCPUCTRL_PADDR,
            GIC_VCPUCTRL_PPTR,
            VMKernelOnly,
            vm_attributes_new(
                false,
                false,
                false
            )
        );
    }

    if (config_set(CONFIG_ARM_SMMU)) {
        map_kernel_frame(
            MC_PADDR,
            SMMU_PPTR,
            VMKernelOnly,
            vm_attributes_new(
                false,
                false,
                false
            )
        );
    }

#ifdef CONFIG_PRINTING
    /* map kernel device: UART */
    map_kernel_frame(
        UARTA_PADDR,
        UARTA_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif
}


/* co-processor code to read and write GPT */

static void
write_cntp_ctl(uint32_t v)
{
    asm volatile ("mcr p15, 0, %0, c14, c2, 1" ::"r"(v));
}

static void
write_cntp_tval(uint32_t v)
{
    asm volatile  ("mcr p15, 0, %0, c14, c2, 0" :: "r"(v));
}

static uint32_t
read_cntfrq(void)
{
    uint32_t val;
    asm volatile ("mrc  p15, 0, %0, c14, c0, 0" : "=r"(val));
    return val;
}


static void
write_cnthp_ctl(uint32_t v)
{
    asm volatile ("mcr p15, 4, %0, c14, c2, 1" ::"r"(v));
}

static void
write_cnthp_tval(uint32_t v)
{
    asm volatile  ("mcr p15, 4, %0, c14, c2, 0" :: "r"(v));
}

#define GPT_DEFAULT_HZ		12000000
static uint32_t gpt_cnt_tval = 0;

/**
   DONT_TRANSLATE
 */
void
resetTimer(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        write_cnthp_tval(gpt_cnt_tval);
    } else {
        write_cntp_tval(gpt_cnt_tval);
    }
}

/**
   DONT_TRANSLATE
 */

/* we use the physical count-down timer of the GPT as the kernel preemption timer */
BOOT_CODE void
initTimer(void)
{
    uint32_t freq = read_cntfrq();
    uint64_t tval = 0;
    if (freq != GPT_DEFAULT_HZ) {
        printf("Default timer has a different frequency %x\n", freq);
    }
    tval = (uint64_t)CONFIG_TIMER_TICK_MS * (freq / 1000);
    if (tval > 0xffffffff) {
        printf("timer interval value out of range \n");
        halt();
    }

    gpt_cnt_tval = (uint32_t)tval;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        write_cnthp_tval(gpt_cnt_tval);
        write_cnthp_ctl(0x1);
    } else {
        /* write the value */
        write_cntp_tval(gpt_cnt_tval);
        /* enable the timer */
        write_cntp_ctl(0x1);
    }
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}


