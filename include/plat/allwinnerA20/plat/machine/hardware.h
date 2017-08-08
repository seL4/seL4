/*
 * Copyright 2015, DornerWorks, Ltd.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#include <util.h>
#include <basic_types.h>
#include <linker.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>

#define physBase          0x48000000
#define kernelBase        0xA0000000

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    /*  GP Timer 11 */
    {
        TIMER0_PADDR,
        TIMER0_PPTR,
        true  /* armExecuteNever */
    },
    /*  GIC */
    {
        GIC_CONTROLLER0_PADDR,
        GIC_CONTROLLER_PPTR,
        true  /* armExecuteNever */
#ifdef CONFIG_PRINTING
    },
    {
        /*  UART */
        UART0_PADDR,
        UART0_PPTR,
        true  /* armExecuteNever */
#endif
    }
};

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 1408 MB */
    { .start = 0x48000000, .end = 0xA0000000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    /* sorted by increasing memory address */
    /* region caps must be a power of 2. */

    /* TODO: Add more devices. */
    { SPI0_PADDR                    , SPI0_PADDR                        + ( 2 << PAGE_BITS) },
    { SPI1_PADDR                    , SPI1_PADDR                        + ( 2 << PAGE_BITS) },

};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
}

#endif
