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
    return numerator / denominator;
}

