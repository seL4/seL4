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

#include <mode/machine.h>
#include <arch/machine/fpu.h>
#include <mode/model/statedata.h>

bool_t isFPUEnabledCached[CONFIG_MAX_NUM_NODES];

#ifdef CONFIG_HAVE_FPU
/* Initialise the FP/SIMD for this machine. */
BOOT_CODE bool_t
fpsimd_init(void)
{
    /* Set the FPU to lazy switch mode */
    disableFpu();
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        enableFpuEL01();
    }

    return true;
}
#endif /* CONFIG_HAVE_FPU */

BOOT_CODE bool_t
fpsimd_HWCapTest(void)
{
    word_t id_aa64pfr0;

    /* Check if the hardware has FP and ASIMD support... */
    MRS("id_aa64pfr0_el1", id_aa64pfr0);
    if ((id_aa64pfr0 >> ID_AA64PFR0_EL1_FP) & MASK(4) ||
            (id_aa64pfr0 >> ID_AA64PFR0_EL1_ASIMD) & MASK(4)) {
        return false;
    }

    return true;
}
