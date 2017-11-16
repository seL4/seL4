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
#include <linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>
#include <plat/machine/timer.h>

#define TIMER0_OFFSET       0xC00

#define TIMER_CTL_EN_FLAG               BIT(0)
#define TIMER_CTL_RELOAD_FLAG           BIT(1)

#define TMR0_IRQ_EN_FLAG            BIT(0)

timer_t *timer = (timer_t *) TIMER0_PPTR + TIMER0_OFFSET;

/* Configure gptimer11 as kernel preemption timer */
BOOT_CODE void
initTimer(void)
{
    /* Set the reload value */
    timer->tmr0_intv_value_reg = TIMER_RELOAD;

    /* Enables interrupt */
    timer->tmr_irq_en_reg = TMR0_IRQ_EN_FLAG;

    /* Set autoreload and start the timer */
    timer->tmr0_ctrl_reg = TIMER_CTL_EN_FLAG | TIMER_CTL_RELOAD_FLAG;
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateCache(void) {}
