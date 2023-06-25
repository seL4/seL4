/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

#if !defined(CONFIG_PLAT_ZYNQMP) && !defined(PLAT_ZYNQMP_ZCU102)
#error "unknown platform"
#endif

#include <sel4/arch/constants_cortex_a53.h>

#if CONFIG_WORD_SIZE == 32
/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xe0000000
#else
/* otherwise this is defined at the architecture level */
#endif
