/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
enum benchmark_track_util_ipc_index {
    /* TCB cap passed in the syscall */
    /* Number of cycles thread spends scheduled */
    BENCHMARK_TCB_UTILISATION,
    /* Number of times thread is scheduled */
    BENCHMARK_TCB_NUMBER_SCHEDULES,
    /* Number of cycles thread spends in kernel */
    BENCHMARK_TCB_KERNEL_UTILISATION,
    /* Number of times thread enters kernel */
    BENCHMARK_TCB_NUMBER_KERNEL_ENTRIES,

    /* Idle thread */
    /* Number of cycles idle thread spends scheduled */
    BENCHMARK_IDLE_LOCALCPU_UTILISATION,
    BENCHMARK_IDLE_TCBCPU_UTILISATION,
    /* Number of times idle thread is scheduled */
    BENCHMARK_IDLE_NUMBER_SCHEDULES,
    /* Number of cycles idle thread spends in kernel */
    BENCHMARK_IDLE_KERNEL_UTILISATION,
    /* Number of times idle thread enters kernel */
    BENCHMARK_IDLE_NUMBER_KERNEL_ENTRIES,

    /* Totals for the current core */
    /* Total cycles used by the core for the period */
    BENCHMARK_TOTAL_UTILISATION,
    /* Total number of times the core schedules a different thread */
    BENCHMARK_TOTAL_NUMBER_SCHEDULES,
    /* Total cycles spent inside the kernel by the core for the period */
    BENCHMARK_TOTAL_KERNEL_UTILISATION,
    /* Total number of times the kernel is entered on the current core */
    BENCHMARK_TOTAL_NUMBER_KERNEL_ENTRIES,
};

#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
