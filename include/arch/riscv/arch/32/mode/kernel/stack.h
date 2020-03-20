/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>

#ifdef ENABLE_SMP_SUPPORT
#define KERNEL_STACK_ALIGNMENT 4096
#else
#define KERNEL_STACK_ALIGNMENT 4
#endif

