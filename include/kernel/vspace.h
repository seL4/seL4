/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __KERNEL_VSPACE_H
#define __KERNEL_VSPACE_H

#include <config.h>
#include <arch/kernel/vspace.h>

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
exception_t benchmark_arch_map_logBuffer(word_t frame_cptr);
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

#endif
