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

#define physBase          0x80000000
#define kernelBase        0xe0000000

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

/* Available physical memory regions on platform (RAM) */
/* NOTE: Regions are not allowed to be adjacent! */
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 1 GiB */
#ifdef CONFIG_ENABLE_BENCHMARKS
#warning "NOTE: logging is currently untested on iMX7 Sabre"
    /* 1MB stolen for logging */
    { /* .start = */ 0x80000000, /* .end = */ 0x9fd00000 }
#else
    { /* .start = */ 0x80000000, /* .end = */ 0xc0000000 }
#endif /* CONFIG_ENABLE_BENCHMARKS */
};

const p_region_t BOOT_RODATA dev_p_regs[] = {
    { GPIO1_PADDR,              GPIO1_PADDR             +   0x10000 },
    { GPIO2_PADDR,              GPIO2_PADDR             +   0x10000 },
    { GPIO3_PADDR,              GPIO3_PADDR             +   0x10000 },
    { GPIO4_PADDR,              GPIO4_PADDR             +   0x10000 },
    { GPIO5_PADDR,              GPIO5_PADDR             +   0x10000 },
    { GPIO6_PADDR,              GPIO6_PADDR             +   0x10000 },
    { GPIO7_PADDR,              GPIO7_PADDR             +   0x10000 },
    { WDOG1_PADDR,              WDOG1_PADDR             +   0x10000 },
    { WDOG2_PADDR,              WDOG2_PADDR             +   0x10000 },
    { WDOG3_PADDR,              WDOG3_PADDR             +   0x10000 },
    { WDOG4_PADDR,              WDOG4_PADDR             +   0x10000 },
    { IOMUXC_LPSR_PADDR,        IOMUXC_LPSR_PADDR       +   0x10000 },
    { GPT1_PADDR,               GPT1_PADDR              +   0x10000 },
    { GPT2_PADDR,               GPT2_PADDR              +   0x10000 },
    { GPT3_PADDR,               GPT3_PADDR              +   0x10000 },
    { GPT4_PADDR,               GPT4_PADDR              +   0x10000 },
    { IOMUXC_PADDR,             IOMUXC_PADDR            +   0x10000 },
    { IOMUXC_GPR_PADDR,         IOMUXC_GPR_PADDR        +   0x10000 },
    { OCOTP_CTRL_PADDR,         OCOTP_CTRL_PADDR        +   0x10000 },
    { ANALOG_PADDR,             ANALOG_PADDR            +   0x10000 },
    { CCM_PADDR,                CCM_PADDR               +   0x10000 },
    { SRC_PADDR,                SRC_PADDR               +   0x10000 },
    { PWM1_PADDR,               PWM1_PADDR              +   0x10000 },
    { PWM2_PADDR,               PWM2_PADDR              +   0x10000 },
    { PWM3_PADDR,               PWM3_PADDR              +   0x10000 },
    { PWM4_PADDR,               PWM4_PADDR              +   0x10000 },
    { UART1_PADDR,              UART1_PADDR             +   0x10000 },
    { UART2_PADDR,              UART2_PADDR             +   0x10000 },
    { UART3_PADDR,              UART3_PADDR             +   0x10000 },
    { UART4_PADDR,              UART4_PADDR             +   0x10000 },
    { UART5_PADDR,              UART5_PADDR             +   0x10000 },
    { UART6_PADDR,              UART6_PADDR             +   0x10000 },
    { UART7_PADDR,              UART7_PADDR             +   0x10000 },
    { I2C1_PADDR,               I2C1_PADDR              +   0x10000 },
    { I2C2_PADDR,               I2C2_PADDR              +   0x10000 },
    { I2C3_PADDR,               I2C3_PADDR              +   0x10000 },
    { I2C4_PADDR,               I2C4_PADDR              +   0x10000 },
    { USB1_OTG_PADDR,           USB1_OTG_PADDR          +   0x10000 },
    { USB2_OTG_PADDR,           USB2_OTG_PADDR          +   0x10000 },
    { USB_HOST_PADDR,           USB_HOST_PADDR          +   0x10000 },
    { USDHC1_PADDR,             USDHC1_PADDR            +   0x10000 },
    { USDHC2_PADDR,             USDHC2_PADDR            +   0x10000 },
    { USDHC3_PADDR,             USDHC3_PADDR            +   0x10000 },
    { ENET1_PADDR,              ENET1_PADDR             +   0x10000 },
    { ENET2_PADDR,              ENET2_PADDR             +   0x10000 },
};

/* Handle a platform-reserved IRQ. */
static inline void
handleReservedIRQ(irq_t irq)
{
}

#endif /* !__PLAT_MACHINE_HARDWARE_H */
