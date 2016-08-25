/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
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
