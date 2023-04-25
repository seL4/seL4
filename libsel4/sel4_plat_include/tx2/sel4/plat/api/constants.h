/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

/* Actually, it's a NVIDIA-Denver2/A57 HMP big.LITTLE system */
#if defined(CONFIG_ARM_CORTEX_A57)
#include <sel4/arch/constants_cortex_a57.h>
#else
#error "unsupported core"
#endif
