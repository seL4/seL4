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
        /*  EPIT */
        EPIT_PADDR,
        EPIT_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  AVIC */
        AVIC_PADDR,
        AVIC_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  L2CC */
        L2CC_PADDR,
        L2CC_PPTR,
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
/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

static const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 128 MiB of memory minus kernel image at its beginning */
    { .start = 0x80000000, .end = 0x88000000 }
};

static const p_region_t BOOT_RODATA dev_p_regs[] = {
    { .start = 0x43f80000, .end = 0x43f81000 }, /* IMX31 I2C 1 */
    { .start = 0x43f84000, .end = 0x43f85000 }, /* IMX31 I2C 3 */
    { .start = 0x43f88000, .end = 0x43f89000 }, /* IMX31 USBOTG */
    { .start = 0x43f8c000, .end = 0x43f8d000 }, /* IMX31 ATA control */
    { .start = UART_PADDR, .end = UART_PADDR + BIT(PAGE_BITS) }, /* IMX31 UART 1 */
    { .start = 0x43f94000, .end = 0x43f95000 }, /* IMX31 UART 2 */
    { .start = 0x43f98000, .end = 0x43f99000 }, /* IMX31 I2C 2 */
    { .start = 0x43f9c000, .end = 0x43f9d000 }, /* IMX31 1-WIRE */
    { .start = 0x43fa0000, .end = 0x43fa1000 }, /* IMX31 SSI 1 */
    { .start = 0x43fa4000, .end = 0x43fa5000 }, /* IMX31 CSPI 1 */
    { .start = 0x43fa8000, .end = 0x43fa9000 }, /* IMX31 KPP */
    { .start = 0x43fac000, .end = 0x43fad000 }, /* IMX31 IOMUXC */
    { .start = 0x43fb0000, .end = 0x43fb1000 }, /* IMX31 UART 4 */
    { .start = 0x43fb4000, .end = 0x43fb5000 }, /* IMX31 UART 5 */

    /* The devices from here up to the SPBA are all accessed through the
     * SPBA (bus arbiter), which must be configured to attach them to either
     * the CPU or the DMA controller. The reset state is to disable everything
     * (except the SPBA itself). This may need to be handled by the kernel, if
     * the kernel is managing the SDMA controller.
     */
    { .start = 0x50004000, .end = 0x50005000 }, /* IMX31 SDHC 1 */
    { .start = 0x50008000, .end = 0x50009000 }, /* IMX31 SDHC 2 */
    { .start = 0x5000c000, .end = 0x5000d000 }, /* IMX31 UART 3 */
    { .start = 0x50010000, .end = 0x50011000 }, /* IMX31 CSPI 2 */
    { .start = 0x50014000, .end = 0x50015000 }, /* IMX31 SSI 2 */
    { .start = 0x50018000, .end = 0x50019000 }, /* IMX31 SIM */
    { .start = 0x5001c000, .end = 0x5001d000 }, /* IMX31 IIM */
    { .start = 0x50020000, .end = 0x50021000 }, /* IMX31 ATA (DMA) */
    { .start = 0x50024000, .end = 0x50025000 }, /* IMX31 MSHC 1 */
    { .start = 0x50028000, .end = 0x50029000 }, /* IMX31 MSHC 2 */
    { .start = 0x5003c000, .end = 0x5003d000 }, /* IMX31 SPBA */

    { .start = 0x53f80000, .end = 0x53f81000 }, /* IMX31 CCM */
    { .start = 0x53f84000, .end = 0x53f85000 }, /* IMX31 CSPI 3 */
    { .start = 0x53f8c000, .end = 0x53f8d000 }, /* IMX31 FIR */
    { .start = 0x53f90000, .end = 0x53f91000 }, /* IMX31 GPT */
    /* EPIT 1 is used by the kernel */
    { .start = 0x53f98000, .end = 0x53f99000 }, /* IMX31 EPIT 2 */
    { .start = 0x53fa4000, .end = 0x53fa8000 }, /* IMX31 GPIO 3 */
    /* This is disabled until we know what it does */
    //{ .start = 0x53fac000, .end = 0x53fad000 }, /* IMX31 SCC */
    { .start = 0x53fb0000, .end = 0x53fb1000 }, /* IMX31 RNGA */
    /* Disabled, because it has an internal DMA controller */
    //{ .start = 0x53fc0000, .end = 0x53fc1000 }, /* IMX31 IPU */
    { .start = 0x53fc4000, .end = 0x53fc5000 }, /* IMX31 AUDMUX */
    /* Disabled, because it has an internal DMA controller */
    //{ .start = 0x53fc8000, .end = 0x53fc9000 }, /* IMX31 MPEG4enc */
    { .start = 0x53fcc000, .end = 0x53fd0000 }, /* IMX31 GPIO 1 */
    { .start = 0x53fd0000, .end = 0x53fd4000 }, /* IMX31 GPIO 2 */
    /* This can access any physical address; the kernel should control it */
    //{ .start = 0x53fd4000, .end = 0x53fd5000 }, /* IMX31 SDMA */
    { .start = 0x53fd8000, .end = 0x53fd9000 }, /* IMX31 RTC */
    /* This can reset the machine if not regularly serviced; it should
     * be disabled or controlled by the kernel
     */
    //{ .start = 0x53fdc000, .end = 0x53fde000 }, /* IMX31 WDOG */
    { .start = 0x53fe0000, .end = 0x53fe1000 }, /* IMX31 PWM */
    { .start = 0x53fec000, .end = 0x53fed000 }, /* IMX31 RTIC */
    { .start = 0xa0000000, .end = 0xa4000000 }, /* IMX31 CS0 (flash) */
    { .start = 0xa8000000, .end = 0xaa000000 }, /* IMX31 CS1 (flash) */
    { .start = 0xb4000000, .end = 0xb6000000 }, /* IMX31 CS4 (FPGA) */
    { .start = 0xb6000000, .end = 0xb8000000 }, /* IMX31 CS5 (enet) */
    { .start = 0xc0000000, .end = 0xc2000000 }  /* IMX31 PCMCIA/CF */
};

#endif
