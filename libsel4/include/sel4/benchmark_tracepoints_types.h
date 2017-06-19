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

#ifndef BENCHMARK_TRACE_POINTS_TYPES_H
#define BENCHMARK_TRACE_POINTS_TYPES_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#ifdef CONFIG_BENCHMARK_TRACEPOINTS
typedef struct benchmark_tracepoint_log_entry {
    seL4_Word  id;
    seL4_Word  duration;
} benchmark_tracepoint_log_entry_t;
#endif /* CONFIG_BENCHMARK_TRACEPOINTS */

#endif /* BENCHMARK_TRACE_POINTS_TYPES_H */
