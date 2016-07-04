/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

#ifdef CONFIG_ENABLE_BENCHMARKS

#include <arch/benchmark.h>

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
uint64_t ccnt_num_overflows;
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

void
armv_init_ccnt(void)
{
    uint32_t pmcr = 0;

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    /* Enable generating interrupts on overflows */
    pmcr = BIT(6);
    asm volatile (
        "mcr p15, 0, %0, c15, c12, 0\n"
        :
        : "r" (pmcr)
    );
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

    /* enable them */
    pmcr |= BIT(2) | BIT(0);
    asm volatile (
        "mcr p15, 0, %0, c15, c12, 0\n"
        : /* no outputs */
        : "r" (pmcr)
    );
}

#endif /* CONFIG_ENABLE_BENCHMARKS */
