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
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <plat/machine/devices_gen.h>
#include <arch/benchmark_overflowHandler.h>

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  GIC controller and private timers */
        ARM_MP_PADDR,
        ARM_MP_PPTR1,
        true  /* armExecuteNever */
    },
    {
        /*  GIC distributor */
        ARM_MP_PADDR + BIT(PAGE_BITS),
        ARM_MP_PPTR2,
        true  /* armExecuteNever */
    },
    {
        /*  L2CC */
        L2CC_PL310_PADDR,
        L2CC_PL310_PPTR,
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

static inline void
handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    if (irq == KERNEL_PMU_IRQ) {
        handleOverflowIRQ();
    }
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
}

#endif /* !__PLAT_MACHINE_HARDWARE_H */
