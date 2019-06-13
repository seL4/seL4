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

#include <config.h>
#ifdef CONFIG_ENABLE_BENCHMARKS

#define CCNT "p15, 0, %0, c15, c12, 1"
#define PMCR "p15, 0, %0, c15, c12, 0"

/* these numbers are taking from the document
 * "Application Note 195
 *  ARM11 performance monitor unit"
 */
#define PMCR_CCNT_IRQ 6
#define PMCR_OVERFLOW_FLAG 10

static inline void armv_enableOverflowIRQ(void)
{
    word_t val;
    MRC(PMCR, val);
    val |= BIT(PMCR_CCNT_IRQ);
    MCR(PMCR, val);
}

static inline void armv_handleOverflowIRQ(void)
{
    word_t pmcr;
    MRC(PMCR, pmcr);
    pmcr |= PMCR_OVERFLOW_FLAG;
    MCR(PMCR, pmcr);
}
#endif /* CONFIG_ENABLE_BENCHMARKS */
#endif /* ARMV_BENCHMARK_H */
