/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 */

#ifdef CONFIG_ENABLE_BENCHMARK
// RVTODO: #error in benchmark.h implies that this code cannot be used and
// may not be correct

#include <benchmark.h>
#include <arch/benchmark.h>

uint32_t ksEntry;
uint32_t ksExit;
uint32_t ksLogIndex = 0;
uint32_t *ksLog;

#endif /* CONFIG_ENABLE_BENCHMARK */


