/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <stdint.h>

#ifdef CONFIG_KERNEL_MCS
#include <util.h>

/* timer function definitions that work for all 32bit arm platforms that provide
 * CLK_MAGIC and TIMER_CLOCK_MHZ -- these definitions might need to move
 * if we come across an arm platform that does not suit this model */

/* Get the max. ticks_t value that can be expressed in time_t (time in us). This
 * is the max. value ticksToUs() can be passed without overflowing.
 */
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

