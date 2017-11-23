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

#include <config.h>
#ifdef CONFIG_ENABLE_BENCHMARKS

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

static inline void benchmark_arch_utilisation_reset(void)
{
}

#endif /* CONFIG_ENABLE_BENCHMARKS */
#endif /* ARCH_BENCHMARK_H */
