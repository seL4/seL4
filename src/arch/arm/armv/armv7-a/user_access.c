/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <plat/machine/hardware.h>
#include <arch/user_access.h>
#include <mode/machine/debug.h>

#define PMUSERENR_ENABLE BIT(0)

#define CNTKCTL_PL0PCTEN BIT(0)
#define CNTKCTL_PL0VCTEN BIT(1)
#define CNTKCTL_PL0VTEN  BIT(8)
#define CNTKCTL_PL0PTEN  BIT(9)

#define ID_DFR0_PMU_MASK (0xful << 28)
#define ID_DFR0_PMU_NONE (0xful << 28)

#define ID_PFR1_GENERIC_TIMER BIT(16)

#define ID_PFR0_JAZELLE_MASK (0xful << 8)  /* ID_PFR0.State2 */
#define ID_PFR0_THUMBEE_MASK (0xful << 12) /* ID_PFR0.State3 */

#define JOSCR_CD  BIT(1)
#define TEECR_XED BIT(0)
#define HSTR_TTEE  BIT(16)
#define HSTR_TJDBX BIT(17)


static void disable_jazelle_user_access(void)
{
    uint32_t v;
    word_t UNUSED hstr_bits = 0;  /* Only read in CONFIG_ARM_HYPERVISOR_SUPPORT */

    MRC(ID_PFR0, v);
    if (v & ID_PFR0_JAZELLE_MASK) {
        /* When JOSCR.CD == 0, JMCR is readable and writable for user space.
         * Clear, disable, and trap access. */
        MCR(JMCR, 0);
        MCR(JOSCR, JOSCR_CD);
        hstr_bits |= HSTR_TJDBX;
    }
    if (v & ID_PFR0_THUMBEE_MASK) {
        /* Same as above, but for TEECR.XED == 0 and TEEHBR */
        MCR(TEEHBR, 0);
        MCR(TEECR, TEECR_XED);
        hstr_bits |= HSTR_TTEE;
    }
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    /* Trap access to config registers so guest cannot re-enable them. */
    setHSTR(getHSTR() | hstr_bits);
#endif
}


static void check_export_pmu(void)
{
#if defined CONFIG_EXPORT_PMU_USER || defined CONFIG_ENABLE_BENCHMARKS
    /* Export performance counters */
    uint32_t v;
    MRC(PMUSERENR, v);
    v |= PMUSERENR_ENABLE;
    MCR(PMUSERENR, v);

    /* enable user-level pmu event counter if we're in secure mode */
    if (!(readDscrCp() & DBGDSCR_SECURE_MODE_DISABLED)) {
        MRC(DBGSDER, v);
        v |= DBGSDER_ENABLE_SECURE_USER_NON_INVASIVE_DEBUG;
        MCR(DBGSDER, v);
    }
#endif
}


static void check_export_arch_timer(void)
{
    uint32_t v = 0;
#ifdef CONFIG_EXPORT_PCNT_USER
    v |= CNTKCTL_PL0PCTEN;
#endif
#ifdef CONFIG_EXPORT_PTMR_USER
    v |= CNTKCTL_PL0PTEN;
#endif /* CONFIG_EXPORT_PTMR_USER */
#ifdef CONFIG_EXPORT_VCNT_USER
    v |= CNTKCTL_PL0VCTEN;
#endif
#ifdef CONFIG_EXPORT_VTMR_USER
    v |= CNTKCTL_PL0VTEN;
#endif /* CONFIG_EXPORT_VTMR_USER */
    MCR(CNTKCTL, v);
}


void armv_init_user_access(void)
{
    uint32_t v;

    /* Disable Jazelle DBX and Jazelle RCT (ThumbEE) */
    disable_jazelle_user_access();
    /* Performance Monitoring Unit */
    MRC(ID_DFR0, v);
    if ((v & ID_DFR0_PMU_MASK) != ID_DFR0_PMU_NONE) {
        check_export_pmu();
    }
    /* Arch timers */
    MRC(ID_PFR1, v);
    if (v & ID_PFR1_GENERIC_TIMER) {
        check_export_arch_timer();
    }
}

