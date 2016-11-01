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
#define __MODE_MACHINE_TIMER_H

#include <config.h>
#include <types.h>
#include <arch/model/statedata.h>

static inline CONST uint64_t
div64(uint64_t numerator, uint32_t denominator)
{
    uint64_t quotient = 0llu;
    uint64_t long_denom = (uint64_t) denominator;

    if (unlikely(denominator > numerator)) {
        return 0;
    }

    assert(numerator > 0);
    assert(denominator > 0);

    /* align denominator to numerator */
    uint64_t c = ((uint64_t) 32u + clzl(denominator)) - clzll(numerator);
    long_denom = long_denom << c;

    /* perform binary long division */
    while (c < UINT64_MAX) {
        if (numerator >= long_denom) {
            numerator -= long_denom;
            quotient |= (1llu << c);
        }
        c--;
        long_denom = long_denom >> 1llu;
    }

    return quotient;
}

static inline PURE time_t
getMaxTimerUs(void)
{
    return div64(UINT64_MAX, x86KStscMhz);
}

static inline PURE time_t
ticksToUs(ticks_t ticks)
{
    return div64(ticks, x86KStscMhz);
}

#endif /* __MODE_MACHINE_TIMER_H */
