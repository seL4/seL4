/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

const p_region_t BOOT_RODATA avail_p_regs[] = {
    { .start = 0x20000000, .end = 0x28000000 }
};

BOOT_CODE int get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_avail_p_reg(unsigned int i)
{
    return avail_p_regs[i];
}

const p_region_t BOOT_RODATA dev_p_regs[] = {
    { /* .start = */ UART0_PADDR,    /* .end = */ UART0_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART1_PADDR,    /* .end = */ UART1_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART2_PADDR,    /* .end = */ UART2_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART3_PADDR,    /* .end = */ UART3_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ UART4_PADDR,    /* .end = */ UART4_PADDR + (1 << PAGE_BITS) },
    { /* .start = */ GIC_PADDR,      /* .end = */ GIC_PADDR + ((1 << PAGE_BITS) * 8) },
    { /* .start = */ DMTIMER0_PADDR, /* .end = */ DMTIMER0_PADDR + (1 << PAGE_BITS) },
};

BOOT_CODE int get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_dev_p_reg(unsigned int i)
{
    return dev_p_regs[i];
}

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
    printf("Received reserved IRQ: %d\n", (int)irq);
}

BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: GIC */
    map_kernel_frame(
        GIC_CONTROLLER0_PADDR,
        GIC_CONTROLLER_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
    map_kernel_frame(
        GIC_DISTRIBUTOR_PADDR,
        GIC_DISTRIBUTOR_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

#ifdef CONFIG_PRINTING
    /* map kernel device: UART */
    map_kernel_frame(
        UART0_PADDR,
        UART0_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif
}

#define TIMER_CLOCK_HZ		1200000
#define TIMER_RELOAD_VAL	(TIMER_CLOCK_HZ * CONFIG_TIMER_TICK_MS / 1000)

/**
   DONT_TRANSLATE
 */
void
resetTimer(void)
{
    MCR(CNTV_TVAL, TIMER_RELOAD_VAL);
    MCR(CNTV_CTL, (1 << 0));
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    MCRR(CNTV_CVAL, 0xffffffffffffffff);
    resetTimer();
}

void
initL2Cache(void)
{

}
