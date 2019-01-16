/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
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
        /* map kernel device: GIC */
        GIC_CONTROLLER_PADDR,
        GIC_CONTROLLER_PPTR,
        true  /* armExecuteNever */
    },
    {
        GIC_DISTRIBUTOR_PADDR,
        GIC_DISTRIBUTOR_PPTR,
        true  /* armExecuteNever */
#ifdef CONFIG_PRINTING
    },
    {
        /* UART */
        UARTA_PADDR,
        UART_PPTR,
        true
#endif /* CONFIG_PRINTING */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    },
    {
        GICH_PADDR,
        GICH_PPTR,
        true
#endif
    }
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
    if ((config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) && (irq == INTERRUPT_VGIC_MAINTENANCE)) {
        VGICMaintenance();
        return;
    }
    printf("spurious irq %d\n", (int)irq);
}

#endif /* __PLAT_MACHINE_HARDWARE_H */
