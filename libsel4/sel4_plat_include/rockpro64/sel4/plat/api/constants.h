/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>

/* The ROCKPro64 uses a RK3399 SOC with a A53/A72 big.LITTLE design. Both cores
 * are designed to be identical
 */
#if defined(CONFIG_ARM_CORTEX_A53)
#include <sel4/arch/constants_cortex_a53.h>
#elif defined(CONFIG_ARM_CORTEX_A72)
#include <sel4/arch/constants_cortex_a72.h>
#else
#error "unsupported core"
#endif
