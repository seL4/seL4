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

#include <plat/machine/hardware.h>
#include <arch/user_access.h>

#define EL0VCTEN BIT(1)
#define EL0PCTEN BIT(0)

static void check_export_pmu(void)
{
#ifdef CONFIG_EXPORT_PMU_USER
    /* allow PL1 to access the PMU */
    uint32_t val = PMUSERENR_EL0_EN;
    MSR("PMUSERENR_EL0", val);
#endif
}

static void check_export_arch_timer(void)
{
    uint32_t val;
    MRS("CNTKCTL_EL1", val);
#ifdef CONFIG_EXPORT_PCNT_USER
    val |= EL0PCTEN;
#endif
#ifdef CONFIG_EXPORT_VCNT_USER
    val |= EL0VCTEN;
#endif
    MSR("CNTKCTL_EL1", val);
}

void
armv_init_user_access(void)
{
    check_export_pmu();
    check_export_arch_timer();
}
