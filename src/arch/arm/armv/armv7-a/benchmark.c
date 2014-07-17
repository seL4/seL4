/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifdef CONFIG_BENCHMARK

#include <arch/benchmark.h>

void
armv_init_ccnt(void)
{
    uint32_t val, pmcr;

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
    val = (1U << 31);
    asm volatile (
        "mcr p15, 0, %0, c9, c12, 1\n"
        : /* no outputs */
        : "r" (val)
    );
}

#endif /* CONFIG_BENCHMARK */
