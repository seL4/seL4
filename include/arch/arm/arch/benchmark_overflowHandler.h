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
#ifndef ARCH_BENCHMARK_OV_H
#define ARCH_BENCHMARK_OV_H

#include <config.h>
#ifdef CONFIG_ENABLE_BENCHMARKS

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
#include <model/statedata.h>
#include <armv/benchmark.h>
#include <armv/benchmark_irqHandler.h>

extern bool_t benchmark_log_utilisation_enabled;

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
static inline void handleOverflowIRQ(void)
{
    if (likely(benchmark_log_utilisation_enabled)) {
        ksCurThread->benchmark.utilisation += 0xFFFFFFFFU - ksCurThread->benchmark.schedule_start_time;
        ksCurThread->benchmark.schedule_start_time = 0;

        ccnt_num_overflows++;
        armv_handleOverflowIRQ();
    }
}
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */
#endif /* ARCH_BENCHMARK_OV_H */
