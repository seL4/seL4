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

#include <config.h>
#ifdef CONFIG_ENABLE_BENCHMARKS

#include <armv/benchmark.h>
#include <mode/machine.h>

void armv_init_ccnt(void);

static inline timestamp_t
timestamp(void)
{
    timestamp_t ccnt;
    SYSTEM_READ_WORD(CCNT, ccnt);
    return ccnt;
}
#endif /* CONFIG_ENABLE_BENCHMARKS */

#endif /* ARCH_BENCHMARK_H */
