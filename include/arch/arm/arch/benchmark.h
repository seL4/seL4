/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#ifdef CONFIG_ENABLE_BENCHMARKS

#include <armv/benchmark.h>
#include <mode/machine.h>
#include <model/statedata.h>

/* these values are consistent across all arm PMUs */
#define PMCR_ENABLE 0
#define PMCR_ECNT_RESET 1
#define PMCR_CCNT_RESET 2

#if defined(CONFIG_BENCHMARK_TRACK_UTILISATION) && defined(KERNEL_PMU_IRQ)
#define CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT 1
#endif

void arm_init_ccnt(void);

static inline timestamp_t timestamp(void)
{
    timestamp_t ccnt;
    SYSTEM_READ_WORD(CCNT, ccnt);
    return ccnt;
}

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
static inline void handleOverflowIRQ(void)
{
    if (likely(NODE_STATE(benchmark_log_utilisation_enabled))) {
        NODE_STATE(ksCurThread)->benchmark.utilisation += UINT32_MAX - NODE_STATE(ksCurThread)->benchmark.schedule_start_time;
        NODE_STATE(ksCurThread)->benchmark.schedule_start_time = 0;
        NODE_STATE(ccnt_num_overflows)++;
    }
    armv_handleOverflowIRQ();
}
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

static inline void benchmark_arch_utilisation_reset(void)
{
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    NODE_STATE(ccnt_num_overflows) = 0;
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
}
#endif /* CONFIG_ENABLE_BENCHMARKS */

