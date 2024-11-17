/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <mode/machine.h>
#include <arch/machine/fpu.h>
#include <mode/model/statedata.h>

#ifdef CONFIG_HAVE_FPU
/* Initialise the FP/SIMD for this machine. */
BOOT_CODE bool_t fpsimd_init(void)
{
    /* Set the FPU to lazy switch mode */
    disableFpu();
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        enableFpuEL01();
    }

    return true;
}
#endif /* CONFIG_HAVE_FPU */

BOOT_CODE bool_t fpsimd_HWCapTest(void)
{
    word_t id_aa64pfr0;

    /* Check if the hardware has FP and ASIMD support... */
    MRS("id_aa64pfr0_el1", id_aa64pfr0);
    if (((id_aa64pfr0 >> ID_AA64PFR0_EL1_FP) & MASK(4)) == MASK(4) ||
        ((id_aa64pfr0 >> ID_AA64PFR0_EL1_ASIMD) & MASK(4)) == MASK(4)) {
        return false;
    }

    return true;
}
