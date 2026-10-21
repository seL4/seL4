/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <mode/machine.h>
#include <arch/machine/fpu.h>
#include <mode/model/statedata.h>
#include <config.h>
#include <util.h>

/*
 * The following function checks if the subarchitecture support asynchronous exceptions
 */
BOOT_CODE static inline bool_t supportsAsyncExceptions(void)
{
    word_t fpexc;

    /* Set FPEXC.EX=1 */
    VMRS(FPEXC, fpexc);
    fpexc |= BIT(FPEXC_EX_BIT);
    VMSR(FPEXC, fpexc);

    /* Read back the FPEXC register*/
    VMRS(FPEXC, fpexc);

    return !!(fpexc & BIT(FPEXC_EX_BIT));
}

#ifdef CONFIG_HAVE_FPU
/* This variable is set at boot/init time to true if the FPU supports 32 registers (d0-d31).
 * otherwise it only supports 16 registers (d0-d15).
 * We cache this value in the following variable to avoid reading the coprocessor
 * on every FPU context switch, since it shouldn't change for one platform on run-time.
 */
bool_t isFPUD32SupportedCached;

BOOT_CODE static inline bool_t isFPUD32Supported(void)
{
    word_t mvfr0;
    asm volatile(".word 0xeef73a10 \n"   /* vmrs    r3, mvfr0 */
                 "mov %0, r3       \n"
                 : "=r"(mvfr0)
                 :
                 : "r3");
    return ((mvfr0 & 0xf) == 2);
}

/* Initialise the FP/SIMD for this machine. */
BOOT_CODE bool_t fpsimd_init(void)
{
    /* We are inside CONFIG_HAVE_FPU, so CPACR_DEFAULT enables CP10 and CP11 access. */
    setCPACR(CPACR_DEFAULT);
    isb();

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* Allow hyp mode FPU accesses for the rest of the initialisation. */
        enableFpuInstInHyp();
    }

    if (supportsAsyncExceptions()) {
        /* In the future, when we've targets that support asynchronous FPU exceptions, we've to support them */
        printf("Error: seL4 doesn't support FPU subarchitectures that support asynchronous exceptions\n");
        return false;
    }

    isFPUD32SupportedCached = isFPUD32Supported();
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* FPEXC is VCPU state and native threads always have EN set. Setting EN
         * must happen before disableFpu() traps hyp mode (kernel) FPU accesses
         * again. */
        setEnFPEXC();
    }
    /* Set the FPU to lazy switch mode */
    disableFpu();

    return true;
}
#endif /* CONFIG_HAVE_FPU */

/* Probe the FPU hardware. Requires CP10 and CP11 access in CPACR and, in hyp
 * mode, no CP10 and CP11 traps in HCPTR. */
BOOT_CODE static bool_t fpsimd_HWCapProbe(void)
{
    word_t fpsid;

    /* Check of this platform supports HW FP instructions */
    asm volatile(".word 0xeef00a10  \n"  /* vmrs    r0, fpsid */
                 "mov %0, r0        \n"
                 : "=r"(fpsid) :
                 : "r0");
    if (fpsid & BIT(FPSID_SW_BIT)) {
        return false;
    }

    word_t fpsid_subarch;

    if (supportsAsyncExceptions()) {
        /* In the future, when we've targets that support asynchronous FPU exceptions, we've to support them */
        if (config_set(CONFIG_HAVE_FPU)) {
            printf("Error: seL4 doesn't support FPU subarchitectures that support asynchronous exceptions\n");
            return false;
        } else {
            // if we aren't using the fpu then we have detected an fpu that we cannot use, but that is fine
            return true;
        }
    }
    /* Check for subarchitectures we support */
    fpsid_subarch = (fpsid >> FPSID_SUBARCH_SHIFT_POS) & 0x7f;

    switch (fpsid_subarch) {
    /* We only support the following subarch values */
    case 0x2:
    case 0x3:
    case 0x4:
        break;
    default: {
        if (config_set(CONFIG_HAVE_FPU)) {
            printf("Error: seL4 doesn't support this VFP subarchitecture\n");
            return false;
        } else {
            // if we aren't using the fpu then we have detected an fpu that we cannot use, but that is fine
            return true;
        }
    }

    }

    if (!config_set(CONFIG_HAVE_FPU)) {
        printf("Info: Not using supported FPU as FPU is disabled in the build configuration\n");
    }
    return true;
}

BOOT_CODE bool_t fpsimd_HWCapTest(void)
{
    bool_t result;

    /* Enable CP10 and CP11 access to read the FPU control/status registers. */
    setCPACR(CPACR_FPU_ENABLE);
    isb();

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    /* Allow hyp mode (kernel) FPU accesses, then restore HCPTR afterwards. */
    word_t hcptr = getHCPTR();
    setHCPTR(hcptr & ~HCPTR_FPU_TRAPS);
#endif

    result = fpsimd_HWCapProbe();

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    setHCPTR(hcptr);
#endif

    /* Restore the CPACR value for native threads. Without CONFIG_HAVE_FPU this
     * disables CP10 and CP11 access again. */
    setCPACR(CPACR_DEFAULT);
    isb();

    return result;
}
