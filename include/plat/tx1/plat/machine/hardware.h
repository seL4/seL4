/*
 * Copyright 2016, Data61
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
#include <arch/linker.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>

#define TIMER_CLOCK_HZ      19200000llu

#define physBase            0x80000000
#define kernelBase          0xffffff8000000000

/* Maximum virtual address accessible from userspace */
#define USER_TOP            0x0000ffffffffffff

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
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    { .start = 0x80000000, .end = 0xa7f00000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    { UARTA_SYNC_PADDR,     UARTA_SYNC_PADDR + (BIT(seL4_PageBits) * 3 ) },    /* 12 KB, multiple */
    { TMR_PADDR,            TMR_PADDR + BIT(seL4_PageBits) }                   /* 4 Kb            */
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{

}

#endif /* __PLAT_MACHINE_HARDWARE_H */
