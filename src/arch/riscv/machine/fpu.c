/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifdef CONFIG_HAVE_FPU
#include <arch/machine/fpu.h>

bool_t isFPUEnabledCached[CONFIG_MAX_NUM_NODES];
#endif
