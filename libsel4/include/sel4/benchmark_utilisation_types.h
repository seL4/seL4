/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef BENCHMARK_TRACK_UTIL_TYPES_H
#define BENCHMARK_TRACK_UTIL_TYPES_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
enum benchmark_track_util_ipc_index {
    BENCHMARK_TCB_UTILISATION,
    BENCHMARK_IDLE_UTILISATION,
    BENCHMARK_TOTAL_UTILISATION
};

#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* BENCHMARK_TRACK_UTIL_TYPES_H */
