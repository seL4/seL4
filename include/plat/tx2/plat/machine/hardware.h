/*
 * Copyright 2018, Data61
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

#define physBase            0x81000000

/*
 * Physical addresses are supported up to 34 bits (0x3FFFFFFFF).
 * These addresses are split up like so:
 * Always MMIO (0.0G – 0.5G)
 * RESERVED (0.5G – 0.75G)
 * Always SysRAM (0.75G – 1.0G)
 * Reclaimable PCIe (1G – 2G)
 * Always DRAM (2G – 16G)
 *
 * We create untypeds for this range.
 */
#define PADDR_USER_DEVICE_TOP BIT(34)

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
        UARTA_PPTR,
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

#define PHYSICAL_RAM_START physBase
#define PHYSICAL_CARVEOUT_START 0xf0000000
#define PHYSICAL_CARVEOUT_END 0xf0110000
#define PHYSICAL_RAM_END 0x276000000

/* Available physical memory regions on platform
 * NOTE: Regions are not allowed to be adjacent!
 */
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    { .start = PHYSICAL_RAM_START, .end = PHYSICAL_CARVEOUT_START },
    /* This gap is due to memory used by Microcode Carveout */
    { .start = PHYSICAL_CARVEOUT_END, .end = PHYSICAL_RAM_END },
};

/*
 * Device regions that get given to user level as device untypeds.
 * We pass through every address apart from the physical memory region
 * and the ARM peripheral region which includes GIC and other interrupt
 * controller registers.
 */
static const p_region_t BOOT_RODATA dev_p_regs[] = {
    {0, ARM_PERIPHBASE},
    {ARM_PERIPHBASE + ARM_PERIPHBASE_SIZE, PHYSICAL_RAM_START},
    {PHYSICAL_RAM_END, PADDR_USER_DEVICE_TOP}
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
    printf("spurious irq %d\n", (int)irq);
}

#endif /* __PLAT_MACHINE_HARDWARE_H */
