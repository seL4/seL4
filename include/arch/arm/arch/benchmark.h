/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef ARCH_BENCHMARK_H
#define ARCH_BENCHMARK_H

#if CONFIG_MAX_NUM_TRACE_POINTS > 0

#include <armv/benchmark.h>

typedef uint32_t timestamp_t;

void armv_init_ccnt(void);

#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */

#endif /* ARCH_BENCHMARK_H */
