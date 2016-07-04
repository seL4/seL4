/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef ARMV_BENCHMARK_IRQ_H
#define ARMV_BENCHMARK_IRQ_H

#ifdef CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT

static inline void armv_handleOverflowIRQ(void)
{
    uint32_t val;
    /* Clear the overflow flag */
    val = BIT(31);
    asm volatile ("mcr p15, 0, %0, c9, c12, 3;" : : "r" (val));
}
#endif /* CONFIG_ARM_ENABLE_PMU_OVERFLOW_INTERRUPT */
#endif /* ARMV_BENCHMARK_IRQ_H */
