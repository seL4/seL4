/*
 * Copyright 2023, NIO
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

#if defined(CONFIG_ARM_CORTEX_A78AE)
#include <sel4/arch/constants_cortex_a78ae.h>
#else
#error "unsupported core"
#endif
