/*
 * Copyright 2015, DornerWorks, Ltd.
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
    /* map kernel device: GP Timer 11 */
    map_kernel_frame(
        TIMER0_PADDR,
        TIMER0_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

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

#define TIMER_INTERVAL_US  (CONFIG_TIMER_TICK_MS * 1000)
#define TIMER_MHZ          24ULL
#define TIMER_TICKS        (TIMER_MHZ * TIMER_INTERVAL_US)

#define TIMER0_OFFSET       0xC00

#define TIMER_CTL_EN_FLAG               BIT(0)
#define TIMER_CTL_RELOAD_FLAG           BIT(1)

#define TMR0_IRQ_EN_FLAG            BIT(0)
#define TMR0_IRQ_PEND_FLAG          BIT(0)

static volatile struct TIMER_map {
    uint32_t tmr_irq_en_reg;        /* Timer IRQ Enable Register 0x00 */
    uint32_t tmr_irq_sta_reg;       /* Timer Status Register  0x04 */
    uint32_t tmr_reserved01[2];
    uint32_t tmr0_ctrl_reg;         /* Timer 0 Control Register  0x10 */
    uint32_t tmr0_intv_value_reg;   /* Timer 0 Interval Value Register 0x14 */
    uint32_t tmr0_cur_value_reg;    /* Timer 0 Current Value Register  0x18 */
} *timer = (volatile void*)(TIMER0_PPTR + TIMER0_OFFSET);

/**
   DONT_TRANSLATE
 */
void
resetTimer(void)
{
    timer->tmr_irq_sta_reg = TMR0_IRQ_PEND_FLAG;
}

/* Configure gptimer11 as kernel preemption timer */
/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    /* Set the reload value */
    timer->tmr0_intv_value_reg = TIMER_TICKS;

    /* Enables interrupt */
    timer->tmr_irq_en_reg = TMR0_IRQ_EN_FLAG;

    /* Set autoreload and start the timer */
    timer->tmr0_ctrl_reg = TIMER_CTL_EN_FLAG | TIMER_CTL_RELOAD_FLAG;
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

