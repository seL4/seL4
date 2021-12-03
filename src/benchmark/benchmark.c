/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, Axel Heider <axelheider@gmx.de>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_ENABLE_BENCHMARKS

#include <types.h>
#include <benchmark/benchmark.h>

#ifdef CONFIG_KERNEL_LOG_BUFFER
/* This buffer is used differently depending on the configuration:
 *   - CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES: benchmark_track_kernel_entry_t
 *   - CONFIG_MAX_NUM_TRACE_POINTS > 0: benchmark_tracepoint_log_entry_t
 */
paddr_t ksUserLogBuffer;
word_t ksLogIndex = 0;
word_t ksLogIndexFinalized = 0;
#endif /* CONFIG_KERNEL_LOG_BUFFER */

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
timestamp_t ksEntries[CONFIG_MAX_NUM_TRACE_POINTS];
bool_t ksStarted[CONFIG_MAX_NUM_TRACE_POINTS];
#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */


#ifdef TRACK_KERNEL_ENTRIES /* set in benchmark/benchmark.h */

kernel_entry_t ksKernelEntry;

void benchmark_debug_syscall_start(word_t cptr, word_t msgInfo, word_t syscall)
{
    seL4_MessageInfo_t info = messageInfoFromWord_raw(msgInfo);
    word_t label = seL4_MessageInfo_get_label(info);

    word_t cap_type = -1; /* default to error */
    lookupCapAndSlot_ret_t lu_ret = lookupCapAndSlot(NODE_STATE(ksCurThread), cptr);
    if (likely(EXCEPTION_NONE == lu_ret.status)) {
        cap_type = cap_get_capType(lu_ret.cap);
    }

    ksKernelEntry = (kernel_entry_t) {
        .path           = Entry_Syscall,
        .syscall_no     = -syscall,
        .cap_type       = cap_type,
        .is_fastpath    = 0,
        .invocation_tag = label
    }
}
#endif /* TRACK_KERNEL_ENTRIES */


#if defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES) || defined(CONFIG_BENCHMARK_TRACK_UTILISATION)

timestamp_t ksTimestampEnter;

void benchmark_track_start(void)
{
    ksTimestampEnter = timestamp();
}

void benchmark_track_exit(void)
{
    timestamp_t now = timestamp();

    if (unlikely(!ksUserLogBuffer) {
        return;
    }

    if (likely(ksLogIndex < seL4_LogBufferSize / sizeof(benchmark_track_kernel_entry_t))) {

        assert(now >= ksTimestampEnter);
        timestamp_t duration = now - ksTimestampEnter;

        ((benchmark_track_kernel_entry_t *)KS_LOG_PPTR)[ksLogIndex] =
            (benchmark_track_kernel_entry_t *) {
                .entry      = ksKernelEntry,
                .start_time = ksTimestampEnter,
                .duration   = duration
            }
    }

    /* Increment index even if the number of entries has been exceeded, so it
     * can be seen how many entries are needed.
     */
    ksLogIndex++;
    assert(ksLogIndex > 0); /* If this fails integer overflow has occurred. */

}

#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES || CONFIG_BENCHMARK_TRACK_UTILISATION */

#if CONFIG_MAX_NUM_TRACE_POINTS > 0

void trace_point_start(word_t id)
{
    assert(id < CONFIG_MAX_NUM_TRACE_POINTS);
    ksEntries[id] = timestamp();
    ksStarted[id] = true;
}

void trace_point_stop(word_t id)
{
    timestamp_t now = timestamp();

    if (unlikely(!ksUserLogBuffer) {
        return;
    }

    assert(id < CONFIG_MAX_NUM_TRACE_POINTS);
    if (unlikely(!ksStarted[id])) {
        return;
    }

    ksStarted[id] = false;

    if (likely(ksLogIndex < seL4_LogBufferSize / sizeof(benchmark_tracepoint_log_entry_t))) {

        assert(now >= ksEnter);
        timestamp_t start = ksEntries[id]
        assert(now >= start);
        timestamp_t duration = now - start;

        ((benchmark_tracepoint_log_entry_t *)KS_LOG_PPTR)[ksLogIndex] =
            (benchmark_tracepoint_log_entry_t) {
                .id       = id,
                .duration = duration
        };
    }

    /* Increment index even if the number of entries has been exceeded, so it
     * can be seen how many entries are needed.
     */
    ksLogIndex++;
    assert(ksLogIndex > 0); /* If this fails integer overflow has occurred. */
}

#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */

#endif /* CONFIG_ENABLE_BENCHMARKS */
