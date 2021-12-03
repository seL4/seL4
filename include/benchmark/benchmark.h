/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_ENABLE_BENCHMARKS

#include <types.h>

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES)
#define TRACK_KERNEL_ENTRIES 1
extern kernel_entry_t ksKernelEntry;
void benchmark_debug_syscall_start(word_t cptr, word_t msgInfo, word_t syscall);
#endif /* CONFIG_DEBUG_BUILD || CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
void benchmark_track_start(void);
void benchmark_track_exit(void);
#else /* not CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */
#define benchmark_track_start(void) do {} while(0)
#define benchmark_track_exit(void) do {} while(0)
#endif /* [not] CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
void trace_point_start(word_t id);
#define TRACE_POINT_START(x)  trace_point_start(x)
void trace_point_stop(word_t id);
#define TRACE_POINT_STOP(x)   trace_point_stop(x)
#else /* not CONFIG_MAX_NUM_TRACE_POINTS > 0 */
#define TRACE_POINT_START(x)    do {} while(0)
#define TRACE_POINT_STOP(x)     do {} while(0)
#endif /* [not] CONFIG_MAX_NUM_TRACE_POINTS > 0 */

#enidf /* CONFIG_ENABLE_BENCHMARKS */
