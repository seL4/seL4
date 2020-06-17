/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/benchmark.h>
#include <sel4/benchmark_utilisation_types.h>
#include <sel4/arch/constants.h>
#include <model/statedata.h>

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
extern bool_t benchmark_log_utilisation_enabled;
extern timestamp_t ksEnter;
extern timestamp_t benchmark_start_time;
extern timestamp_t benchmark_end_time;

void benchmark_track_utilisation_dump(void);

void benchmark_track_reset_utilisation(void);
/* Calculate and add the utilisation time from when the heir started to run i.e. scheduled
 * and until it's being kicked off
 */
static inline void benchmark_utilisation_switch(tcb_t *heir, tcb_t *next)
{
    /* Add heir thread utilisation */
    if (likely(benchmark_log_utilisation_enabled)) {

        /* Check if an overflow occurred while we have been in the kernel */
        if (likely(ksEnter > heir->benchmark.schedule_start_time)) {

            heir->benchmark.utilisation += (ksEnter - heir->benchmark.schedule_start_time);

        } else {
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
            heir->benchmark.utilisation += (UINT32_MAX - heir->benchmark.schedule_start_time) + ksEnter;
            armv_handleOverflowIRQ();
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
        }

        /* Reset next thread utilisation */
        next->benchmark.schedule_start_time = ksEnter;

    }
}

/* Add the time between the last thread got scheduled and when to stop
 * benchmarks
 */
static inline void benchmark_utilisation_finalise(void)
{
    /* Add the time between when NODE_STATE(ksCurThread), and benchmark finalise */
    benchmark_utilisation_switch(NODE_STATE(ksCurThread), NODE_STATE(ksIdleThread));

    benchmark_end_time = ksEnter;
    benchmark_log_utilisation_enabled = false;
}

#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
