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

#define PMCR_CYCLE_COUNT_RESET BIT(2)
#define PMCR_EVENT_COUNT_RESET BIT(1)
#define PMCR_ENABLE BIT(0)

#define PMCNTENSET_CYCLE_COUNT_ENABLE BIT(31)

#define PMUSERENR_EL0_EN BIT(0)

#define CCNT "PMCCNTR_EL0"

#endif /* ARMV_BENCHMARK_H */
