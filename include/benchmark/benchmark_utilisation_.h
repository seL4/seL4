/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <basic_types.h>

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
typedef struct {
    timestamp_t schedule_start_time;
    uint64_t    utilisation;
    uint64_t    number_schedules;
    uint64_t    kernel_utilisation;
    uint64_t    number_kernel_entries;

} benchmark_util_t;
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */

