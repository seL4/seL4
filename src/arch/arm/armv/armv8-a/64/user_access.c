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

/* bits in the CNTKCTL_EL1 */
#define EL0VCTEN BIT(1)
#define EL0PCTEN BIT(0)
#define EL0VTEN  BIT(8)
#define EL0PTEN  BIT(9)

/* bits in CNTHCTL_EL2 */
#define EL1PCEN  BIT(1)
#define EL1PCTEN BIT(0)

#define PMUSERENR_EL0_EN BIT(0)

static void check_export_pmu(void)
{
#if defined CONFIG_EXPORT_PMU_USER || defined CONFIG_ENABLE_BENCHMARKS
    /* allow PL0 to access the PMU */
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
#endif /* CONFIG_EXPORT_PCNT_USER */
#ifdef CONFIG_EXPORT_PTMR_USER
    val |= EL0PTEN;
#endif /* CONFIG_EXPORT_PTMR_USER */
#ifdef CONFIG_EXPORT_VCNT_USER
    val |= EL0VCTEN;
#endif /* CONFIG_EXPORT_VCNT_USER */
#ifdef CONFIG_EXPORT_VTMR_USER
    val |= EL0VTEN;
#endif /* CONFIG_EXPORT_VTMR_USER */
    MSR("CNTKCTL_EL1", val);

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    MRS("CNTHCTL_EL2", val);
#ifdef CONFIG_EXPORT_PCNT_USER
    val |= EL1PCTEN;
#endif /* CONFIG_EXPORT_PCNT_USER */
#ifdef CONFIG_EXPORT_PTMR_USER
    val |= EL1PCEN;
#endif /* CONFIG_EXPORT_PTMR_USER */
    MSR("CNTHCTL_EL2", val);
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
}

void armv_init_user_access(void)
{
    check_export_pmu();
    check_export_arch_timer();
}
