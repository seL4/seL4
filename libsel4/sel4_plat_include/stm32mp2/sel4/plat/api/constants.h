/*
 * Copyright 2026, STMicroelectronics
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/arch/constants_cortex_a35.h>

#ifdef CONFIG_ARCH_AARCH32
/* First address in the virtual address space that is not accessible to user level */
#error "AARCH32 is unsupported"
#else
/* otherwise this is defined at the arch level */
#endif
