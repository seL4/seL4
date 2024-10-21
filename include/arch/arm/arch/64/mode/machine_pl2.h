/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

static inline void writeTPIDR_EL2(register_t reg)
{
    PMSR("tpidr_el2", reg);
}

static inline register_t readTPIDR_EL2(void)
{
    register_t reg;
    PMRS("tpidr_el2", reg);
    return reg;
}

#else

static inline void writeTPIDR_EL2(register_t reg) {}
static inline register_t readTPIDR_EL2(void)
{
    return 0;
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */

/* used in other files without guards */
static inline void setCurrentPDPL2(paddr_t pa) {}
static inline void invalidateHypTLB(void) {}
static inline void writeContextIDPL2(word_t pd_val) {}
static inline void writeContextIDAndPD(word_t id, word_t pd_val) {}
