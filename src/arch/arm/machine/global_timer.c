/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
#include <arch/machine/global_timer.h>

timer_t * const globalTimer = (timer_t *) ARM_MP_GLOBAL_TIMER_PPTR;

/** DONT_TRANSLATE */
BOOT_CODE void initTimer(void)
{
    /* disable the timer */
    globalTimer->control = 0;
    /* zero the timer */
    globalTimer->countLower = 0;
    globalTimer->countUpper = 0;
    /* turn it on again, wih interrupts on, comparator register off,
     * in one-shot mode, with standard prescaler */
    globalTimer->control = BIT(ENABLE) | BIT(IRQ_ENABLE);

    /* this timer will overflow in about 1000 years */
}
