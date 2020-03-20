/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <arch/model/statedata.h>

static inline CONST uint64_t div64(uint64_t numerator, uint32_t denominator)
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

