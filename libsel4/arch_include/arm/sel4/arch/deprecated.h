/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

#if defined(CONFIG_ARCH_AARCH64) && !defined(CONFIG_ALLOW_SMC_CALLS)
#define CONFIG_ALLOW_SMC_CALLS SEL4_DEPRECATE_MACRO(1)
#endif
