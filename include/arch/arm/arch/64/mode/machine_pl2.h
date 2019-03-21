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

#ifndef __ARCH_MODE_MACHINE_PL2_H
#define __ARCH_MODE_MACHINE_PL2_H

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

static inline void writeTPIDRRO_EL0(word_t reg)
{
    MSR("tpidrro_el0", reg);
}

static inline word_t readTPIDRRO_EL0(void)
{
    word_t reg;
    MRS("tpidrro_el0", reg);
    return reg;
}

static inline void writeTPIDR_EL2(word_t reg)
{
    MSR("tpidr_el2", reg);
}

static inline word_t readTPIDR_EL2(void)
{
    word_t reg;
    MRS("tpidr_el2", reg);
    return reg;
}

#else

static inline void writeTPIDR_EL2(word_t reg) {}
static inline word_t readTPIDR_EL2(void)
{
    return 0;
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */

/* used in other files without guards */
static inline void setCurrentPDPL2(paddr_t pa) {}
static inline void invalidateHypTLB(void) {}
static inline void writeContextIDPL2(word_t pd_val) {}
static inline void writeContextIDAndPD(word_t id, word_t pd_val) {}
static inline paddr_t addressTranslateS1CPR(vptr_t vaddr)
{
    return vaddr;
}

#endif /* __ARCH_MODE_MACHINE_H */
