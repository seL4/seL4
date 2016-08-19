/*
 * Copyright 2016, Data61 CSIRO
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */
#pragma once

#include <config.h>
#include <arch/model/statedata.h>
#include <plat/machine.h>
#include <mode/machine/timer.h>

/* prototypes for mode specific functions -- these require 64 bit
 * division */
static inline PURE time_t ticksToUs(ticks_t ticks);
static inline PURE time_t getMaxTimerUs(void);

/* generic x86 timer functions */
static inline CONST time_t
getKernelWcetUs(void)
{
    return  10u;
}

static inline PURE ticks_t
getTimerPrecision(void)
{
    return return usToTicks(1u);
}

static inline PURE ticks_t
usToTicks(time_t us)
{
    assert(x86KStscMhz > 0);
    assert(us >= getKernelWcetUs() && us <= getMaxTimerUs());
    return us * x86KStscMhz;
}

static inline void
ackDeadlineIRQ(void)
{
}

static inline ticks_t
getCurrentTime(void)
{
    return x86_rdtsc();
}

static inline void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurrentTime);
    //TODO are MSRs different on X86_64? Or shold this be prefixed with X86?
    x86_wrmsr(IA32_TSC_DEADLINE_MSR, deadline);
}
