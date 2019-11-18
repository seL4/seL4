/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <arch/machine/fpu.h>

#ifdef CONFIG_HAVE_FPU
fpu_status_t fpu_status[CONFIG_MAX_NUM_NODES];
#endif
