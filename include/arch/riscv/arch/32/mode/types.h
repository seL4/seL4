/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __ARCH_MODE_TYPES_H
#define __ARCH_MODE_TYPES_H

#include <config.h>
#include <assert.h>
#include <stdint.h>

compile_assert(long_is_32bits, sizeof(unsigned long) == 4)

#define wordRadix 5

#endif
