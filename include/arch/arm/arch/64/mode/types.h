/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <assert.h>

compile_assert(long_is_64bits, sizeof(unsigned long) == 8)

#define wordRadix 6
#define wordBits (1 << wordRadix)

typedef uint64_t timestamp_t;
