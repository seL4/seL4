/*
 * Copyright 2016, CSIRO Data61
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H
#include <config.h>
#include <types.h>
#include <basic_types.h>
#include <arch/linker.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <machine/io.h>
#include <plat/machine/hardware.h>

#define TIMER_CLOCK_HZ		19200000llu

#define physBase          0x00000000
#define kernelBase        0xe0000000

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


/* Available physical memory regions on platform (RAM) */
/* NOTE: Regions are not allowed to be adjacent! */
const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* RPI has 1 GiB but it is split between the videocard and the processor.
        Currently the processor gets 128MiB as according to the current uboot */
    { /* .start = */ 0x00000000, /* .end = */ 0x8000000 }
};

const p_region_t BOOT_RODATA dev_p_regs[] = {
    { /* .start */ SDHC_PADDR     , /* .end */ SDHC_PADDR      + (1u << PAGE_BITS) },
    { /* .start */ USB2_PADDR     , /* .end */ USB2_PADDR      + (1u << PAGE_BITS) },
    { /* .start */ UART_PADDR     , /* .end */ UART_PADDR      + (1u << PAGE_BITS) },
    { /* .start */ TIMER_PADDR    , /* .end */ TIMER_PADDR     + (1u << PAGE_BITS) },
};

/** MODIFIES: [*] */
void initL2Cache(void);

/** MODIFIES: [*] */
static inline void plat_cleanL2Range(paddr_t start, paddr_t end) {}
/** MODIFIES: [*] */
static inline void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
/** MODIFIES: [*] */
static inline void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

#endif /* !__PLAT_MACHINE_HARDWARE_H */
