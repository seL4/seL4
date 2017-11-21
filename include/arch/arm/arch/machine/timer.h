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

#ifndef __ARCH_MACHINE_TIMER_H_
#define __ARCH_MACHINE_TIMER_H_

#include <config.h>
#include <stdint.h>
#include <types.h>
#include <util.h>


#define USE_KHZ (TIMER_CLOCK_HZ % HZ_IN_MHZ > 0)
#define TIMER_CLOCK_KHZ (TIMER_CLOCK_HZ / HZ_IN_KHZ)
#define TIMER_CLOCK_MHZ (TIMER_CLOCK_HZ / HZ_IN_MHZ)


void initTimer(void);
static inline CONST ticks_t getTimerPrecision(void);
static inline CONST time_t getMaxTimerUs(void);
static inline CONST ticks_t usToTicks(time_t us);
static inline CONST time_t ticksToUs(ticks_t ticks);

#include <plat/machine/timer.h>
#include <mode/machine/timer.h>

static inline CONST ticks_t
usToTicks(time_t us)
{
    if (USE_KHZ) {
        return (us * TIMER_CLOCK_KHZ) / KHZ_IN_MHZ;
    }
    return us * TIMER_CLOCK_MHZ;
}


#endif /* __ARCH_MACHINE_TIMER_H_ */
