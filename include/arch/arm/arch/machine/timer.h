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

#define USE_KHZ (TIMER_CLOCK_HZ % HZ_IN_MHZ > 0)
#define TIMER_CLOCK_KHZ (TIMER_CLOCK_HZ  / HZ_IN_KHZ)
#define TIMER_CLOCK_MHZ (TIMER_CLOCK_HZ /  HZ_IN_MHZ)

/* prototypes for timer functions */
static inline CONST ticks_t getTimerPrecision(void);
static inline CONST time_t getMaxTimerUs(void);
static inline CONST ticks_t usToTicks(time_t us);
static inline CONST time_t ticksToUs(ticks_t ticks);

#include <plat/machine/timer.h>
#include <mode/machine/timer.h>
