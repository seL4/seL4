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

void arm_init_ccnt(void);

static inline timestamp_t timestamp(void)
{
    timestamp_t ccnt;
    SYSTEM_READ_WORD(CCNT, ccnt);
    return ccnt;
}

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
extern bool_t benchmark_log_utilisation_enabled;
extern uint64_t ccnt_num_overflows;
static inline void handleOverflowIRQ(void)
{
    if (likely(benchmark_log_utilisation_enabled)) {
        NODE_STATE(ksCurThread)->benchmark.utilisation += UINT32_MAX - NODE_STATE(ksCurThread)->benchmark.schedule_start_time;
        NODE_STATE(ksCurThread)->benchmark.schedule_start_time = 0;
        ccnt_num_overflows++;
    }
    armv_handleOverflowIRQ();
}

static inline void benchmark_arch_utilisation_reset(void)
{
    ccnt_num_overflows = 0;
}
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
#endif /* CONFIG_ENABLE_BENCHMARKS */

