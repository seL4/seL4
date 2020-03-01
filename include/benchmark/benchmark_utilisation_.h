/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef _BENCHMARK_UTILISATION_H
#define _BENCHMARK_UTILISATION_H

#include <config.h>
#include <basic_types.h>

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
typedef struct {
    timestamp_t schedule_start_time;
    uint64_t    utilisation;
} benchmark_util_t;
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */

#endif /* _BENCHMARK_UTILISATION_H */
