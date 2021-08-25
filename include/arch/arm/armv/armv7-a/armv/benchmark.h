/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef CONFIG_ENABLE_BENCHMARKS
#include <config.h>

#define PMCR "p15, 0, %0, c9, c12, 0"
#define PMCNTENSET "p15, 0, %0, c9, c12, 1"
#define PMOVSR "p15, 0, %0, c9, c12, 3"
#define CCNT "p15, 0, %0, c9, c13, 0"
#define PMINTENSET "p15, 0, %0, c9, c14, 1"
#define CCNT_INDEX 31

static inline void armv_enableOverflowIRQ(void)
{
    word_t val;
    MRC(PMINTENSET, val);
    val |= BIT(CCNT_INDEX);
    MCR(PMINTENSET, val);
}

static inline void armv_handleOverflowIRQ(void)
{
    word_t val = BIT(CCNT_INDEX);
    MCR(PMOVSR, val);
}
#endif /* CONFIG_ENABLE_BENCHMARKS */

