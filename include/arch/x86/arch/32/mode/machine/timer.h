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
#include <machine/io.h>
#include <arch/kernel/apic.h>
#include <arch/model/statedata.h>
#include <arch/linker.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>
#include <plat/machine.h>

static inline CONST uint32_t
clz64(uint64_t n)
{
    uint32_t upper_n = (uint32_t) (n >> 32llu);
    uint32_t lower_n = (uint32_t) n;
    uint32_t lz = 0;

    if (upper_n == 0) {
        assert(lower_n > 0);
        return 32u + clzl(lower_n);
    }

    return clzl(upper_n);
}

static inline CONST uint64_t
div64(uint64_t numerator, uint32_t denominator)
{
    uint64_t c;
    uint64_t quotient;
    uint64_t long_denom;

    quotient = 0llu;
    long_denom = (uint64_t) denominator;

    if (unlikely(denominator > numerator)) {
        return 0;
    }

    assert(numerator > 0);
    assert(denominator > 0);

    /* align denominator to numerator */
    c = 32u + clzl(denominator) - clz64(numerator);
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
