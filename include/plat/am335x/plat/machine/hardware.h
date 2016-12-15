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
#include <plat/machine/interrupt.h>

#define physBase          0x80000000
#define kernelBase        0xf0000000

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  DM Timer 0 */
        DMTIMER0_PADDR,
        DMTIMER0_PPTR,
        true  /* armExecuteNever */
    },
    /*  INTC */
    {
        INTC_PADDR,
        INTC_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  WDT1 */
        WDT1_PADDR,
        WDT1_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  CMPER */
        CMPER_PADDR,
        CMPER_PPTR,
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

/* pointer to end of kernel image */
/* need a fake array to get the pointer from the linker script */
//TODO is this really needed?
extern char ki_end[1];

static const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 128 MiB of memory minus kernel image at its beginning */
    { /* .start = */ (pptr_t)ki_end - (kernelBase - physBase), /* .end = */ 0x88000000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    /* SoC devices: */
    { /* .start = */ UART0_PADDR,    /* .end = */ UART0_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER2_PADDR, /* .end = */ DMTIMER2_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER3_PADDR, /* .end = */ DMTIMER3_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER4_PADDR, /* .end = */ DMTIMER4_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER5_PADDR, /* .end = */ DMTIMER5_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER6_PADDR, /* .end = */ DMTIMER6_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER7_PADDR, /* .end = */ DMTIMER7_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ WDT1_PADDR,     /* .end = */ WDT1_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ CMPER_PADDR,    /* .end = */ CMPER_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ CTRL_PADDR,     /* .end = */ CTRL_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ PRCM_PADDR,     /* .end = */ PRCM_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ CPSW_PADDR,     /* .end = */ CPSW_PADDR + (1 << (PAGE_BITS + 4)) }
    /* Board devices. */
    /* TODO: This should ultimately be replaced with a more general solution. */
};

#endif
