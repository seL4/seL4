/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef ARCH_BENCHMARK_H
#define ARCH_BENCHMARK_H

#ifdef CONFIG_BENCHMARK

#include <armv/benchmark.h>

/* We have 1MB of word sized entries */
#define MAX_LOG_SIZE 262144

extern word_t ksEntry;
extern word_t ksExit;
extern word_t ksLogIndex;
extern word_t *ksLog;


void armv_init_ccnt(void);

#endif /* CONFIG_BENCHMARK */

#endif /* ARCH_BENCHMARK_H */
