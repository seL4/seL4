/*
 * Copyright 2016, General Dynamics C4 Systems
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
#include <plat_mode/machine/hardware.h>

#define physBase            0x20000000

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  GIC */
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
    { .start = 0x20000000, .end = 0x28000000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    { /* .start = */ UART0_PADDR,    /* .end = */ UART0_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART1_PADDR,    /* .end = */ UART1_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART2_PADDR,    /* .end = */ UART2_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART3_PADDR,    /* .end = */ UART3_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART4_PADDR,    /* .end = */ UART4_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ GIC_PADDR,      /* .end = */ GIC_PADDR + ((1 << PAGE_BITS) * 8) },
    { /* .start = */ RTC0_PADDR,     /* .end = */ RTC0_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ RTC1_PADDR,     /* .end = */ RTC1_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER0_PADDR, /* .end = */ DMTIMER0_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER1_PADDR, /* .end = */ DMTIMER1_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER2_PADDR, /* .end = */ DMTIMER2_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER3_PADDR, /* .end = */ DMTIMER3_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER4_PADDR, /* .end = */ DMTIMER4_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER5_PADDR, /* .end = */ DMTIMER5_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER6_PADDR, /* .end = */ DMTIMER6_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER7_PADDR, /* .end = */ DMTIMER7_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ DMTIMER8_PADDR, /* .end = */ DMTIMER8_PADDR + (1 << PAGE_BITS) },
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
}

#endif
