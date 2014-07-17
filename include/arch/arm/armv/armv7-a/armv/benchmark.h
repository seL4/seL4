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

#ifdef CONFIG_BENCHMARK

#ifdef CONFIG_PLAT_IMX6
#define KS_LOG_PADDR 0xffe00000
#else
//TODO implement this for other platforms
#error "Log address unclear and untested!"
#endif

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
#endif /* CONFIG_BENCHMARK */

#endif /* ARMV_BENCHMARK_H */
