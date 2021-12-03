/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, Axel Heider <axelheider@gmx.de>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_ENABLE_BENCHMARKS

#include <types.h>
#include <benchmark/benchmark.h>

#ifdef CONFIG_KERNEL_LOG_BUFFER
/* This buffer is used differently depending on the configuration:
 *   - CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES: benchmark_track_kernel_entry_t
 *   - CONFIG_MAX_NUM_TRACE_POINTS > 0: benchmark_tracepoint_log_entry_t
 */
paddr_t ksUserLogBuffer;
word_t ksLogIndex = 0;
word_t ksLogIndexFinalized = 0;
#endif /* CONFIG_KERNEL_LOG_BUFFER */

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
timestamp_t ksEntries[CONFIG_MAX_NUM_TRACE_POINTS];
bool_t ksStarted[CONFIG_MAX_NUM_TRACE_POINTS];
timestamp_t ksExit;
#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */

#endif /* CONFIG_ENABLE_BENCHMARKS */
