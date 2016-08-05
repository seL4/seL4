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


