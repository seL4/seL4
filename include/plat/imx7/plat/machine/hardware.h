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

#include <basic_types.h>
#include <linker.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <plat/machine/devices_gen.h>

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  GIC distributor and private timers */
        ARM_MP_PADDR,
        ARM_MP_PPTR1,
        true  /* armExecuteNever */
    },
    {
        /*  GIC controller */
        ARM_MP_PADDR + BIT(PAGE_BITS),
        ARM_MP_PPTR2,
        true  /* armExecuteNever */
    },
    {
        /*  GIC controller */
        ARM_MP_PADDR + BIT(PAGE_BITS) * 2,
        ARM_MP_PPTR3,
        true  /* armExecuteNever */

#ifdef CONFIG_PRINTING
    },
    {
        /*  UART */
        UART_PADDR,
        UART_PPTR,
        true  /* armExecuteNever */
#endif
    }
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
}

#endif /* !__PLAT_MACHINE_HARDWARE_H */
