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
#include <plat/machine/devices_gen.h>
#include <plat/machine/hardware_gen.h>
#include <plat/machine/smmu.h>
#include <mode/machine/hardware.h>
#include <mode/api/constants.h>
#include <arch/benchmark_overflowHandler.h>

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
