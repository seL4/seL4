/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */
#pragma once

#include <autoconf.h>

/* This is used for multiple SOCs:
 * - Exynos5250 (A15)
 * - Exynos5410 (A7/A15)
 * - Exynos5422 (A7/A15)
 * Since the A7 was designed as big.LITTLE companion for the A15, their settings
 * are identical.
 */
#if defined(CONFIG_ARM_CORTEX_A7)
#include <sel4/arch/constants_cortex_a7.h>
#elif defined(CONFIG_ARM_CORTEX_A15)
#include <sel4/arch/constants_cortex_a15.h>
#else
#error "unsupported core"
#endif

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xe0000000
