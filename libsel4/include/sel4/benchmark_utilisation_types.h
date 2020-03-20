/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

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

