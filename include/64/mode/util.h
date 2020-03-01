/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __MODE_UTIL_H
#define __MODE_UTIL_H

#include <config.h>
#include <types.h>
#include <arch/model/statedata.h>

static inline CONST uint64_t div64(uint64_t numerator, uint32_t denominator)
{
    return numerator / denominator;
}

#endif /* __MODE_UTIL_H */
