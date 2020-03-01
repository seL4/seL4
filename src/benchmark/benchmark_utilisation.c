/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <benchmark/benchmark_utilisation.h>

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION

bool_t benchmark_log_utilisation_enabled;
timestamp_t ksEnter;
timestamp_t benchmark_start_time;
timestamp_t benchmark_end_time;

void benchmark_track_utilisation_dump(void)
{
    uint64_t *buffer = ((uint64_t *) & (((seL4_IPCBuffer *)lookupIPCBuffer(true, NODE_STATE(ksCurThread)))->msg[0]));
    tcb_t *tcb = NULL;
    word_t tcb_cptr = getRegister(NODE_STATE(ksCurThread), capRegister);
    lookupCap_ret_t lu_ret;
    word_t cap_type;

    lu_ret = lookupCap(NODE_STATE(ksCurThread), tcb_cptr);
    /* ensure we got a TCB cap */
    cap_type = cap_get_capType(lu_ret.cap);
    if (cap_type != cap_thread_cap) {
        userError("SysBenchmarkFinalizeLog: cap is not a TCB, halting");
        return;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(lu_ret.cap));
    buffer[BENCHMARK_TCB_UTILISATION] = tcb->benchmark.utilisation; /* Requested thread utilisation */
    buffer[BENCHMARK_IDLE_LOCALCPU_UTILISATION] = NODE_STATE(
                                                      ksIdleThread)->benchmark.utilisation; /* Idle thread utilisation of current CPU */
#ifdef ENABLE_SMP_SUPPORT
    buffer[BENCHMARK_IDLE_TCBCPU_UTILISATION] = NODE_STATE_ON_CORE(ksIdleThread,
                                                                   tcb->tcbAffinity)->benchmark.utilisation; /* Idle thread utilisation of CPU the TCB is running on */
#else
    buffer[BENCHMARK_IDLE_TCBCPU_UTILISATION] = buffer[BENCHMARK_IDLE_LOCALCPU_UTILISATION];
#endif

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    buffer[BENCHMARK_TOTAL_UTILISATION] =
        (ccnt_num_overflows * 0xFFFFFFFFU) + benchmark_end_time - benchmark_start_time;
#else
    buffer[BENCHMARK_TOTAL_UTILISATION] = benchmark_end_time - benchmark_start_time; /* Overall time */
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

}

void benchmark_track_reset_utilisation(void)
{
    tcb_t *tcb = NULL;
    word_t tcb_cptr = getRegister(NODE_STATE(ksCurThread), capRegister);
    lookupCap_ret_t lu_ret;
    word_t cap_type;

    lu_ret = lookupCap(NODE_STATE(ksCurThread), tcb_cptr);
    /* ensure we got a TCB cap */
    cap_type = cap_get_capType(lu_ret.cap);
    if (cap_type != cap_thread_cap) {
        userError("SysBenchmarkResetThreadUtilisation: cap is not a TCB, halting");
        return;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(lu_ret.cap));

    tcb->benchmark.utilisation = 0;
    tcb->benchmark.schedule_start_time = 0;
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
