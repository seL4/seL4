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

