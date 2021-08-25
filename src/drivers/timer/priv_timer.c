/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/* A9 MPCORE private timer */
#include <machine/timer.h>
#include <arch/machine/timer.h>
#include <drivers/timer/arm_priv.h>

timer_t *const priv_timer = (timer_t *) ARM_MP_PRIV_TIMER_PPTR;

#define TMR_CTRL_ENABLE      BIT(0)
#define TMR_CTRL_AUTORELOAD  BIT(1)
#define TMR_CTRL_IRQEN       BIT(2)
#define TMR_CTRL_PRESCALE    8

#define TIMER_INTERVAL_MS    (CONFIG_TIMER_TICK_MS)
#define TIMER_COUNT_BITS 32

#define PRESCALE ((TIMER_RELOAD) >> TIMER_COUNT_BITS)
#define TMR_LOAD ((TIMER_RELOAD) / (PRESCALE + 1))

BOOT_CODE void initTimer(void)
{
    /* reset */
    priv_timer->ctrl = 0;
    priv_timer->ints = 0;

    /* setup */
    priv_timer->load = TMR_LOAD;
    priv_timer->ctrl |= ((PRESCALE) << (TMR_CTRL_PRESCALE))
                        | TMR_CTRL_AUTORELOAD | TMR_CTRL_IRQEN;

    /* Enable */
    priv_timer->ctrl |= TMR_CTRL_ENABLE;
}
