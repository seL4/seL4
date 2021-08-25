/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

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
