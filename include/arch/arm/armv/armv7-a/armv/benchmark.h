/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef ARMV_BENCHMARK_H
#define ARMV_BENCHMARK_H

#ifdef CONFIG_ENABLE_BENCHMARKS

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
#if defined(CONFIG_PLAT_SABRE) || defined(CONFIG_PLAT_EXYNOS5410)
#define KS_LOG_PADDR 0xffe00000
#else
//TODO test/implement this for other platforms
#error "Log address unclear and untested!"
#endif
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
extern uint64_t ccnt_num_overflows;
static inline void benchmark_arch_utilisation_reset(void)
{
    ccnt_num_overflows = 0;
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */

static inline uint32_t
timestamp(void)
{
    int ret;

    asm volatile (
        "mrc p15, 0, %0, c9, c13, 0\n"
        : "=r" (ret)
    );

    return ret;
}
#endif /* CONFIG_ENABLE_BENCHMARKS */

#endif /* ARMV_BENCHMARK_H */
