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
#ifdef CONFIG_KERNEL_MCS
#include <stdint.h>
#include <util.h>

/* timer function definitions that work for all 64 bit arm platforms */
static inline CONST ticks_t getMaxTicksToUs(void)
{
#if USE_KHZ
    return UINT64_MAX / TIMER_CLOCK_KHZ;
#else
    return UINT64_MAX;
#endif
}

static inline CONST time_t ticksToUs(ticks_t ticks)
{
#if USE_KHZ
    return (ticks * TIMER_CLOCK_KHZ) / TIMER_CLOCK_MHZ;
#else
    return ticks / TIMER_CLOCK_MHZ;
#endif
}
#endif /* CONFIG_KERNEL_MCS */
#endif /*  __ARCH_MODE_MACHINE_TIMER_H_ */
