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
static inline uint32_t
timestamp(void)
{
    /* todo return cycle counter read */
#error "Not implemented"
}

#endif /* CONFIG_BENCHMARK */



#endif /* ARMV_BENCHMARK_H */
