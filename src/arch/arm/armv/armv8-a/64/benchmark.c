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
#include <config.h>
#include <armv/benchmark.h>

#ifdef CONFIG_ENABLE_BENCHMARKS

void armv_init_ccnt(void)
{
    /* enable the PMU and reset the cycle count to 0 */
    uint32_t val = 0;
    MRS("PMCR_EL0", val);
    val |= PMCR_ENABLE | PMCR_EVENT_COUNT_RESET | PMCR_CYCLE_COUNT_RESET;
    MSR("PMCR_EL0", val);

    /* enable the cycle count */
    val = PMCNTENSET_CYCLE_COUNT_ENABLE;
    MSR("PMCNTENSET_EL0", val);

    /* allow PL1 to access the PMU */
    val = PMUSERENR_EL0_EN ;
    MSR("PMUSERENR_EL0", val);
}

#endif
