/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef ARMV_BENCHMARK_H
#define ARMV_BENCHMARK_H

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
#endif /* ARMV_BENCHMARK_H */
