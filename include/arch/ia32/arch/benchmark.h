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

/* we have 4MB of word sized entries */
#define MAX_LOG_SIZE 1048576

extern uint64_t ksEntry;
extern uint64_t ksExit;
extern uint32_t ksLogIndex;
extern uint32_t *ksLog;

#define IA32_KSLOG_IDX 1022

static inline uint64_t
timestamp(void)
{
    uint32_t low, high;

    asm volatile (
        "movl $0, %%eax \n"
        "movl $0, %%ecx \n"
        "cpuid          \n"
        "rdtsc          \n"
        "movl %%edx, %0 \n"
        "movl %%eax, %1 \n"
        "movl $0, %%eax \n"
        "movl $0, %%ecx \n"
        "cpuid          \n"
        : "=r" (high), "=r" (low)
        : /* no inputs */
        : "eax", "ebx", "ecx", "edx"
    );

    return ((uint64_t) high) << 32llu | (uint64_t) low;
}

#endif /* CONFIG_BENCHMARK */
#endif /* ARCH_BENCHMARK_H */
