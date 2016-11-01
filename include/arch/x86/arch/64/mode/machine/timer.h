/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */
#ifndef __MODE_MACHINE_TIMER_H
#define __MODE_MACHINE_TIME_H

#include <config.h>
#include <types.h>
#include <arch/model/statedata.h>

static inline PURE time_t
getMaxTimerUs(void)
{
    return UINT64_MAX / x86KStscMhz;
}

static inline PURE time_t
ticksToUs(ticks_t ticks)
{
    return ticks / x86KStscMhz;
}

#endif /* __MODE_MACHINE_TIME_H */
