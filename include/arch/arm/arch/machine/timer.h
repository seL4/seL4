/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <stdint.h>

/* convert to khz first to avoid overflow */
#define TICKS_PER_MS (TIMER_CLOCK_HZ / HZ_IN_KHZ)

#ifdef CONFIG_KERNEL_MCS
#include <types.h>
#include <util.h>
#include <mode/util.h>

#define USE_KHZ (TIMER_CLOCK_HZ % HZ_IN_MHZ > 0)
#define TIMER_CLOCK_KHZ (TIMER_CLOCK_HZ / HZ_IN_KHZ)
#define TIMER_CLOCK_MHZ (TIMER_CLOCK_HZ / HZ_IN_MHZ)

#include <plat/platform_gen.h>
#include <mode/machine/timer.h>

void initTimer(void);

/* Get the max. time_t value (time in us) that can be expressed in ticks_t. This
 * is the max. value usToTicks() can be passed without overflowing.
 */
static inline CONST time_t getMaxUsToTicks(void)
{
#if USE_KHZ
    return UINT64_MAX / TIMER_CLOCK_KHZ;
#else
    return UINT64_MAX / TIMER_CLOCK_MHZ;
#endif
}

static inline CONST ticks_t usToTicks(time_t us)
{
#if USE_KHZ
    /* reciprocal division overflows too quickly for dividing by KHZ_IN_MHZ.
     * This operation isn't used frequently or on many platforms, so use manual
     * division here */
    return div64(us * TIMER_CLOCK_KHZ, KHZ_IN_MHZ);
#else
    return us * TIMER_CLOCK_MHZ;
#endif
}

static inline CONST ticks_t getTimerPrecision(void)
{
    return usToTicks(TIMER_PRECISION) + TIMER_OVERHEAD_TICKS;
}
#else /* CONFIG_KERNEL_MCS */
#include <mode/machine/timer.h>
#include <plat/machine/hardware.h>

/* but multiply by timer tick ms */
#define TIMER_RELOAD    (TICKS_PER_MS * CONFIG_TIMER_TICK_MS)

#if (TIMER_RELOAD >= UINTPTR_MAX)
#error "Timer reload too high"
#endif

void initTimer(void);
#endif

