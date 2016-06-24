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
    uint32_t val, pmcr;

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT
    /* Enable generating interrupts on overflows */
    val = BIT(31);
    asm volatile (
        "mcr p15, 0, %0, c9, c14, 1\n"
        :
        : "r" (val)
    );
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */

    /* enable them */
    val = 1;
    asm volatile (
        "mcr p15, 0, %0, c9, c14, 0\n"
        :
        : "r" (val)
    );

    /* reset to 0 and make available at user level */
    pmcr = (1 << 2) | 1;
    asm volatile (
        "mcr p15, 0, %0, c9, c12, 0\n"
        : /* no outputs */
        : "r" (pmcr)
    );

    /* turn the cycle counter on */
    val = BIT(31);
    asm volatile (
        "mcr p15, 0, %0, c9, c12, 1\n"
        : /* no outputs */
        : "r" (val)
    );
}

#endif /* CONFIG_ENABLE_BENCHMARKS */
