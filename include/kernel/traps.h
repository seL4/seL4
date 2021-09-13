/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <arch/kernel/traps.h>
#include <smp/lock.h>

/* This C function should be the first thing called from C after entry from
 * assembly. It provides a single place to do any entry work that is not
 * done in assembly for various reasons */
static inline void c_entry_hook(void)
{
    arch_c_entry_hook();
#if defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES) || defined(CONFIG_BENCHMARK_TRACK_UTILISATION)
    ksEnter = timestamp();
#endif
}

/* This C function should be the last thing called from C before exiting
 * the kernel (be it to assembly or returning to user space). It provides
 * a place to provide any additional instrumentation or functionality
 * in C before leaving the kernel */
static inline void c_exit_hook(void)
{
#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
    benchmark_track_exit();
#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */
#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    if (likely(NODE_STATE(benchmark_log_utilisation_enabled))) {
        timestamp_t exit = timestamp();
        NODE_STATE(ksCurThread)->benchmark.number_kernel_entries++;
        NODE_STATE(ksCurThread)->benchmark.kernel_utilisation += exit - ksEnter;
        NODE_STATE(benchmark_kernel_number_entries)++;
        NODE_STATE(benchmark_kernel_time) += exit - ksEnter;
    }
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */

    arch_c_exit_hook();
}

