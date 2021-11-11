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

/* keep some names for legacy compatibility */
#ifdef CONFIG_ARCH_ARM
#if (CONFIG_PHYS_ADDR_SPACE_BITS == 40)
#define CONFIG_ARM_PA_SIZE_BITS_40 1
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define AARCH64_VSPACE_S2_START_L1 CONFIG_AARCH64_VSPACE_S2_START_L1
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
#elif (CONFIG_PHYS_ADDR_SPACE_BITS == 44)
#define CONFIG_ARM_PA_SIZE_BITS_44 1
#endif /* PHYS_ADDR_SPACE_BITS */
#endif /* CONFIG_ARCH_ARM */

/* CONFIG_PADDR_USER_DEVICE_TOP is no longer set by the CMake configuration. */
#define CONFIG_PADDR_USER_DEVICE_TOP CONFIG_PHYS_ADDR_TOP
