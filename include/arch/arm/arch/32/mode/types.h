/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <assert.h>

compile_assert(long_is_32bits, sizeof(unsigned long) == 4)

#define wordRadix 5
#define wordBits (1 << wordRadix)

typedef uint32_t timestamp_t;
