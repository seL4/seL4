/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
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
