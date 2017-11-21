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

#ifndef __ARCH_MODE_MACHINE_TIMER_H_
#define __ARCH_MODE_MACHINE_TIMER_H_

#include <config.h>
#include <stdint.h>

#ifdef CONFIG_KERNEL_MCS
#include <util.h>

/* timer function definitions that work for all 32bit arm platforms that provide
 * CLK_MAGIC and TIMER_CLOCK_MHZ -- these definitions might need to move
 * if we come across an arm platform that does not suit this model */

/* get the max value ticksToUs can be passed without overflowing */
static inline CONST ticks_t getMaxTicksToUs(void)
{
#if USE_KHZ
    return UINT64_MAX / KHZ_IN_MHZ / CLK_MAGIC;
#else
    return UINT64_MAX / CLK_MAGIC;
#endif
}

static inline CONST time_t ticksToUs(ticks_t ticks)
{
    /* simulate 64bit division using multiplication by reciprocal */
#if USE_KHZ
    return (ticks * KHZ_IN_MHZ) * CLK_MAGIC >> CLK_SHIFT;
#else
    return (ticks * CLK_MAGIC) >> CLK_SHIFT;
#endif
}
#endif /* CONFIG_KERNEL_MCS */

#endif /* __ARCH_MODE_MACHINE_TIMER_H_ */
