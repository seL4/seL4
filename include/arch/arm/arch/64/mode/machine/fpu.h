/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <mode/machine/registerset.h>

#ifdef CONFIG_HAVE_FPU
/* Store state in the FPU registers into memory. */
static inline void saveFpuState(tcb_t *thread)
{
    user_fpu_state_t *dest = &thread->tcbArch.tcbContext.fpuState;
    word_t temp;

    asm volatile(
        /* SIMD and floating-point register file */
        ".arch_extension fp\n"
        "stp     q0, q1, [%1, #16 * 0]      \n"
        "stp     q2, q3, [%1, #16 * 2]      \n"
        "stp     q4, q5, [%1, #16 * 4]      \n"
        "stp     q6, q7, [%1, #16 * 6]      \n"
        "stp     q8, q9, [%1, #16 * 8]      \n"
        "stp     q10, q11, [%1, #16 * 10]   \n"
        "stp     q12, q13, [%1, #16 * 12]   \n"
        "stp     q14, q15, [%1, #16 * 14]   \n"
        "stp     q16, q17, [%1, #16 * 16]   \n"
        "stp     q18, q19, [%1, #16 * 18]   \n"
        "stp     q20, q21, [%1, #16 * 20]   \n"
        "stp     q22, q23, [%1, #16 * 22]   \n"
        "stp     q24, q25, [%1, #16 * 24]   \n"
        "stp     q26, q27, [%1, #16 * 26]   \n"
        "stp     q28, q29, [%1, #16 * 28]   \n"
        "stp     q30, q31, [%1, #16 * 30]   \n"

        /* FP control and status registers */
        "mrs     %0, fpsr                   \n"
        "str     %w0, [%1, #16 * 32]        \n"
        "mrs     %0, fpcr                   \n"
        "str     %w0, [%1, #16 * 32 + 4]    \n"
        ".arch_extension nofp\n"
        : "=&r"(temp)
        : "r"(dest)
        : "memory"
    );
}

/* Load FPU state from memory into the FPU registers. */
static inline void loadFpuState(const tcb_t *thread)
{
    const user_fpu_state_t *src = &thread->tcbArch.tcbContext.fpuState;
    word_t temp;

    asm volatile(
        /* SIMD and floating-point register file */
        ".arch_extension fp\n"
        "ldp     q0, q1, [%1, #16 * 0]      \n"
        "ldp     q2, q3, [%1, #16 * 2]      \n"
        "ldp     q4, q5, [%1, #16 * 4]      \n"
        "ldp     q6, q7, [%1, #16 * 6]      \n"
        "ldp     q8, q9, [%1, #16 * 8]      \n"
        "ldp     q10, q11, [%1, #16 * 10]   \n"
        "ldp     q12, q13, [%1, #16 * 12]   \n"
        "ldp     q14, q15, [%1, #16 * 14]   \n"
        "ldp     q16, q17, [%1, #16 * 16]   \n"
        "ldp     q18, q19, [%1, #16 * 18]   \n"
        "ldp     q20, q21, [%1, #16 * 20]   \n"
        "ldp     q22, q23, [%1, #16 * 22]   \n"
        "ldp     q24, q25, [%1, #16 * 24]   \n"
        "ldp     q26, q27, [%1, #16 * 26]   \n"
        "ldp     q28, q29, [%1, #16 * 28]   \n"
        "ldp     q30, q31, [%1, #16 * 30]  \n"

        /* FP control and status registers */
        "ldr     %w0, [%1, #16 * 32]        \n"
        "msr     fpsr, %0                   \n"
        "ldr     %w0, [%1, #16 * 32 + 4]    \n"
        "msr     fpcr, %0                   \n"
        ".arch_extension nofp\n"
        : "=&r"(temp)
        : "r"(src)
        : "memory"
    );
}

/* Disable trapping FPU instructions to EL2 */
static inline void disableTrapFpu(void)
{
    word_t cptr;
    MRS("cptr_el2", cptr);
    cptr &= ~(BIT(10) | BIT(31));
    MSR("cptr_el2", cptr);
    isb();
}

/* Enable FPU access in EL0 and EL1 */
static inline void enableFpuEL01(void)
{
    word_t cpacr;
    MRS("cpacr_el1", cpacr);
    cpacr |= (3 << CPACR_EL1_FPEN);
    MSR("cpacr_el1", cpacr);
    isb();
}

/* Enable the FPU to be used without faulting.
 * Required even if the kernel attempts to use the FPU. */
/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void enableFpu(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        disableTrapFpu();
    } else {
        enableFpuEL01();
    }
}
#endif /* CONFIG_HAVE_FPU */

/* Trap any FPU related instructions to EL2 */
static inline void enableTrapFpu(void)
{
    word_t cptr;
    MRS("cptr_el2", cptr);
    cptr |= (BIT(10) | BIT(31));
    MSR("cptr_el2", cptr);
    isb();
}

/* Disable FPU access in EL0 */
static inline void disableFpuEL0(void)
{
    word_t cpacr;
    MRS("cpacr_el1", cpacr);
    cpacr &= ~(3 << CPACR_EL1_FPEN);
    cpacr |= (1 << CPACR_EL1_FPEN);
    MSR("cpacr_el1", cpacr);
    isb();
}

/* Disable the FPU so that usage of it causes a fault */
static inline void disableFpu(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        enableTrapFpu();
    } else {
        disableFpuEL0();
    }
}

