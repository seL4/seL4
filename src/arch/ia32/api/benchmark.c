/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifdef CONFIG_BENCHMARK

#include <benchmark.h>
#include <arch/benchmark.h>
#include <arch/machine/hardware.h>

DATA_GLOB uint64_t ksEntry;
DATA_GLOB uint64_t ksExit;
DATA_GLOB uint32_t ksLogIndex = 0;
DATA_GLOB uint32_t *ksLog;

#endif /* CONFIG_BENCHMARK */

