/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <benchmark_utilisation.h>

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION

bool_t benchmark_log_utilisation_enabled;
timestamp_t ksEnter;
timestamp_t benchmark_start_time;
timestamp_t benchmark_end_time;

void benchmark_track_utilisation_dump(void)
{
    uint64_t *buffer = ((uint64_t *) & (((seL4_IPCBuffer *)lookupIPCBuffer(true, ksCurThread))->msg[0]));
    tcb_t *tcb = NULL;
    word_t tcb_cptr = getRegister(ksCurThread, capRegister);
    lookupCap_ret_t lu_ret;
    word_t cap_type;

    lu_ret = lookupCap(ksCurThread, tcb_cptr);
    /* ensure we got a TCB cap */
    cap_type = cap_get_capType(lu_ret.cap);
    if (cap_type != cap_thread_cap) {
        userError("SysBenchmarkFinalizeLog: cap is not a TCB, halting");
        return;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(lu_ret.cap));
    buffer[BENCHMARK_TCB_UTILISATION] = tcb->benchmark.utilisation; /* Requested thread utilisation */
    buffer[BENCHMARK_IDLE_UTILISATION] = ksIdleThread->benchmark.utilisation; /* Idle thread utilisation */

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
    word_t tcb_cptr = getRegister(ksCurThread, capRegister);
    lookupCap_ret_t lu_ret;
    word_t cap_type;

    lu_ret = lookupCap(ksCurThread, tcb_cptr);
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
