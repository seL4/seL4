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

uint32_t ksEntry;
uint32_t ksExit;
uint32_t ksLogIndex = 0;
uint32_t *ksLog;

#endif /* CONFIG_BENCHMARK */


