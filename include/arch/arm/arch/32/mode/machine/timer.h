/*
 * Copyright 2016, Data 61, CSIRO
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */
#pragma once

#include <config.h>

#define KHZ_IN_HZ_MAGIC 274877907u
#define KHZ_IN_HZ_SHIFT 38u

/* timer function definitions that work for all 32bit arm platforms that provide
 * CLK_MAGIC and TIMER_CLOCK_MHZ -- these definitions might need to move
 * if we come across an arm platform that does not suit this model */
static inline CONST time_t
getMaxTimerUs(void)
{
    if (USE_KHZ) {
        return UINT64_MAX / KHZ_IN_MHZ / CLK_MAGIC;
    } else {
        return UINT64_MAX / CLK_MAGIC;
    }
}
static inline CONST ticks_t
usToTicks(time_t us)
{
    if (USE_KHZ) {
        /* use Khz - mhz not preceise enough */
        return (us * TIMER_CLOCK_KHZ) * KHZ_IN_HZ_MAGIC >> KHZ_IN_HZ_SHIFT;
    } else {
        return us * TIMER_CLOCK_MHZ;
    }
}

static inline CONST time_t
ticksToUs(ticks_t ticks)
{
    /* simulate 64bit division using multiplication by reciprocal */
    if (USE_KHZ) {
        return (ticks * KHZ_IN_MHZ) * CLK_MAGIC >> CLK_SHIFT;
    } else {
        return (ticks * CLK_MAGIC) >> CLK_SHIFT;
    }
}
