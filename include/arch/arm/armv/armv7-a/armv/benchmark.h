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

#define CCNT "p15, 0, %0, c9, c13, 0"

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
extern uint64_t ccnt_num_overflows;
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
static inline void benchmark_arch_utilisation_reset(void)
{
#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    ccnt_num_overflows = 0;
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */

#endif /* ARMV_BENCHMARK_H */
