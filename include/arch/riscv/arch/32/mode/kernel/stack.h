/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __KERNEL_MODE_STACK_H
#define __KERNEL_MODE_STACK_H

#include <config.h>
#include <util.h>

#ifdef ENABLE_SMP_SUPPORT
#define KERNEL_STACK_ALIGNMENT 4096
#else
#define KERNEL_STACK_ALIGNMENT 4
#endif

#endif /* __KERNEL_MODE_STACK_H */
