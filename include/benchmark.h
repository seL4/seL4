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

#ifdef CONFIG_BENCHMARK
#define TRACE_POINT_START trace_point_start()
#define TRACE_POINT_STOP   trace_point_stop()

/* we can fill the entire IPC buffer except for word 0, which
 * the kernel overwrites with the message tag */
#define MAX_IPC_BUFFER_STORAGE (1024 - 1)

static inline void
trace_point_start(void)
{
    ksEntry = timestamp();
}

static inline void
trace_point_stop(void)
{
    ksExit = timestamp();

    if (likely(ksLogIndex < MAX_LOG_SIZE)) {
        ksLog[ksLogIndex] = ksExit - ksEntry;
    }
    /* increment the log index even if we have exceeded the log size
     * this is so we can tell if we need a bigger log */
    ksLogIndex++;

    /* If this fails integer overflow has occured. */
    assert(ksLogIndex > 0);
}

#else

#define TRACE_POINT_START
#define TRACE_POINT_STOP

#endif /* CONFIG_BENCHMARK */

#endif /* BENCHMARK_H */
