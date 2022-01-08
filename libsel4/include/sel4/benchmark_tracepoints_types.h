/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

#ifdef CONFIG_BENCHMARK_TRACEPOINTS
typedef struct benchmark_tracepoint_log_entry {
    seL4_Word  id;
    seL4_Word  duration;
} benchmark_tracepoint_log_entry_t;
#endif /* CONFIG_BENCHMARK_TRACEPOINTS */
