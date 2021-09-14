/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <assert.h>
#include <stdint.h>

compile_assert(long_is_64bits, sizeof(unsigned long) == 8)

#define wordRadix 6
