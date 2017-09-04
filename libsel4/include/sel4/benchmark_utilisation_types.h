/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef BENCHMARK_TRACK_UTIL_TYPES_H
#define BENCHMARK_TRACK_UTIL_TYPES_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
enum benchmark_track_util_ipc_index {
    BENCHMARK_TCB_UTILISATION,
    BENCHMARK_IDLE_LOCALCPU_UTILISATION,
    BENCHMARK_IDLE_TCBCPU_UTILISATION,
    BENCHMARK_TOTAL_UTILISATION
};

#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* BENCHMARK_TRACK_UTIL_TYPES_H */
