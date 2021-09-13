/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <sel4/config.h>

/* Set ENABLE_SMP_SUPPORT for kernel source files */
#ifdef CONFIG_ENABLE_SMP_SUPPORT
#define ENABLE_SMP_SUPPORT
#endif

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#ifdef CONFIG_ARM_PA_SIZE_BITS_40
#define AARCH64_VSPACE_S2_START_L1
#endif
#endif
