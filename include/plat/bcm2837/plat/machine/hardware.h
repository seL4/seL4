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
#include <types.h>
#include <basic_types.h>
#include <linker.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <plat/machine/devices_gen.h>
#include <machine/io.h>

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /* BCM2837 Interrupt controller */
        INTC_PADDR,
        INTC_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  ARM Interrupt controller ? */
        ARM_LOCAL_PADDR,
        ARM_LOCAL_PPTR,
        true  /* armExecuteNever */
#ifdef CONFIG_PRINTING
    },
    {
        /*  UART */
        UART_PADDR,
        UART_PPTR,
        true  /* armExecuteNever */
#endif /* CONFIG_PRINTING */
    }
};

void initL2Cache(void);

static inline void plat_cleanL2Range(paddr_t start, paddr_t end) {}
static inline void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
static inline void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}
static inline void plat_cleanInvalidateCache(void) {}

#endif /* !__PLAT_MACHINE_HARDWARE_H */
