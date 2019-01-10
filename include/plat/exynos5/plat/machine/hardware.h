/*
 * Copyright 2014, General Dynamics C4 Systems
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
#include <basic_types.h>
#include <linker.h>
#include <arch/object/vcpu.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <plat/machine/devices_gen.h>

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  Watch dog timer used as PIT */
        MCT_PADDR,
        MCT_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  GIC */
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
        false, /* armExecuteNever */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
#ifdef CONFIG_PRINTING
    },
    {
        UART2_PADDR,
        UART_PPTR,
        true  /* armExecuteNever */
#endif /* CONFIG_PRINTING */
    }
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
    if ((config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) &&
            (irq == INTERRUPT_VGIC_MAINTENANCE)) {
        VGICMaintenance();
        return;
    }
}

#endif /* !__PLAT_MACHINE_HARDWARE_H */
