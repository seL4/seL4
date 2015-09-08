/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef BENCHMARK_H
#define BENCHMARK_H

#include <arch/benchmark.h>
#include <machine/io.h>
#include <arch/api/constants.h>

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
#define TRACE_POINT_START(x) trace_point_start(x)
#define TRACE_POINT_STOP(x)   trace_point_stop(x)

typedef struct ks_log_entry {
    uint32_t key;
    uint32_t data;
} ks_log_entry_t;

#define MAX_LOG_SIZE (seL4_LogBufferSize / sizeof(ks_log_entry_t))

extern timestamp_t ksEntries[CONFIG_MAX_NUM_TRACE_POINTS];
extern bool_t ksStarted[CONFIG_MAX_NUM_TRACE_POINTS];
extern timestamp_t ksExit;
extern uint32_t ksLogIndex;
extern uint32_t ksLogIndexFinalized;
extern ks_log_entry_t *ksLog;

/* we can fill the entire IPC buffer except for word 0, which
 * the kernel overwrites with the message tag */
#define MAX_IPC_BUFFER_STORAGE (1024 - 1)

static inline void
trace_point_start(word_t id)
{
    ksEntries[id] = timestamp();
    ksStarted[id] = true;
}

static inline void
trace_point_stop(word_t id)
{
    ksExit = timestamp();

    if (likely(ksStarted[id])) {
        ksStarted[id] = false;
        if (likely(ksLogIndex < MAX_LOG_SIZE)) {
            ksLog[ksLogIndex] = (ks_log_entry_t) {
                id, ksExit - ksEntries[id]
            };
        }
        /* increment the log index even if we have exceeded the log size
         * this is so we can tell if we need a bigger log */
        ksLogIndex++;
    }

    /* If this fails integer overflow has occured. */
    assert(ksLogIndex > 0);
}

#else

#define TRACE_POINT_START(x)
#define TRACE_POINT_STOP(x)

#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */

#endif /* BENCHMARK_H */
