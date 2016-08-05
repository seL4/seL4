/*
 * Copyright 2014, General Dynamics C4 Systems
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

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
    if ((config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) && (irq == INTERRUPT_VGIC_MAINTENANCE)) {
        VGICMaintenance();
        return;
    }
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
            true,  /* armExecuteNever */
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
                false, /* armExecuteNever */
                false, /* armParityEnabled */
                false  /* armPageCacheable */
            )
        );
    }

#ifdef CONFIG_PRINTING
    /* map kernel device: UART */
    map_kernel_frame(
        UART2_PADDR,
        UART_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif /* CONFIG_PRINTING */
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

