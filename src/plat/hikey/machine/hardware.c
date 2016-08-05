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

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
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

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

