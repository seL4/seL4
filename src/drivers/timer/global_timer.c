/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <drivers/timer/arm_global.h>

timer_t *const globalTimer = (timer_t *) TIMER_PPTR;

/** DONT_TRANSLATE */
BOOT_CODE void initTimer(void)
{
    /* disable the timer */
    globalTimer->control = 0;
    /* zero the timer */
    globalTimer->countLower = 0;
    globalTimer->countUpper = 0;
    /* turn it on again, with interrupts on, comparator register off,
     * in one-shot mode, with standard prescaler */
    globalTimer->control = BIT(ENABLE) | BIT(IRQ_ENABLE);

    /* this timer will overflow in about 1000 years */
}
