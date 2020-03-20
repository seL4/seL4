/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#ifdef CONFIG_ENABLE_BENCHMARKS

#define CCNT "PMCCNTR_EL0"
#define PMCR "PMCR_EL0"
#define PMCNTENSET "PMCNTENSET_EL0"
#define PMINTENSET "PMINTENSET_EL1"
#define PMOVSR "PMOVSCLR_EL0"
#define CCNT_INDEX 31

static inline void armv_enableOverflowIRQ(void)
{
    uint32_t val;
    MRS(PMINTENSET, val);
    val |= BIT(CCNT_INDEX);
    MSR(PMINTENSET, val);
}

static inline void armv_handleOverflowIRQ(void)
{
    uint32_t val = BIT(CCNT_INDEX);
    MSR(PMOVSR, val);
}

#endif /* CONFIG_ENABLE_BENCHMARKS */

