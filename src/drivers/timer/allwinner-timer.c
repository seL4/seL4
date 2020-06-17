/*
 * Copyright 2015, DornerWorks, Ltd.
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <linker.h>
#include <plat/machine/hardware.h>

#define TIMER_CTL_EN_FLAG               BIT(0)
#define TIMER_CTL_RELOAD_FLAG           BIT(1)

#define TMR0_IRQ_EN_FLAG            BIT(0)

timer_t *timer = (timer_t *) TIMER0_PPTR;

/* Configure gptimer11 as kernel preemption timer */
BOOT_CODE void initTimer(void)
{
    /* Set the reload value */
    timer->tmr0_intv_value_reg = TIMER_RELOAD;

    /* Enables interrupt */
    timer->tmr_irq_en_reg = TMR0_IRQ_EN_FLAG;

    /* Set autoreload and start the timer */
    timer->tmr0_ctrl_reg = TIMER_CTL_EN_FLAG | TIMER_CTL_RELOAD_FLAG;
}

