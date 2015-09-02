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
#include <arch/machine/hardware.h>
#include <stdbool.h>

DATA_GLOB timestamp_t ksEntries[CONFIG_MAX_NUM_TRACE_POINTS];
DATA_GLOB bool_t ksStarted[CONFIG_MAX_NUM_TRACE_POINTS];
DATA_GLOB timestamp_t ksExit;
DATA_GLOB uint32_t ksLogIndex = 0;
DATA_GLOB uint32_t ksLogIndexFinalized = 0;
DATA_GLOB ks_log_entry_t *ksLog;

#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */

