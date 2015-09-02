/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#if CONFIG_MAX_NUM_TRACE_POINTS > 0

#include <benchmark.h>
#include <arch/benchmark.h>
#include <stdbool.h>

timestamp_t ksEntries[CONFIG_MAX_NUM_TRACE_POINTS];
bool_t ksStarted[CONFIG_MAX_NUM_TRACE_POINTS];
timestamp_t ksExit;
uint32_t ksLogIndex = 0;
uint32_t ksLogIndexFinalized = 0;
ks_log_entry_t *ksLog;

#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */


