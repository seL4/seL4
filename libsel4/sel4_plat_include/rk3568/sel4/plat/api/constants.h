/*
 * Copyright 2025, UNSW
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

#if defined(CONFIG_ARM_CORTEX_A55)
#include <sel4/arch/constants_cortex_a55.h>
#else
#error "unsupported core"
#endif
