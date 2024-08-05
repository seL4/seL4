/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#ifdef CONFIG_KERNEL_MCS
#include <stdint.h>
#include <util.h>

/* timer function definitions that work for all 64 bit arm platforms */

/* Get the max. ticks_t value that can be expressed in time_t (time in us). This
 * is the max. value ticksToUs() can be passed without overflowing.
 */
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
    return (ticks * KHZ_IN_MHZ) / TIMER_CLOCK_KHZ;
#else
    return ticks / TIMER_CLOCK_MHZ;
#endif
}
#endif /* CONFIG_KERNEL_MCS */

