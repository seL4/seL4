/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <mode/machine/registerset.h>
#include <util.h>
#include <mode/machine.h>


#define CPACR_CP_10_SHIFT_POS        20
#define CPACR_CP_11_SHIFT_POS        22
#define CPACR_CP_ACCESS_DISABLE      0x0
#define CPACR_CP_ACCESS_PL1          0x1
#define CPACR_CP_ACCESS_PLX          0x3

#define CPACR_D32DIS_BIT             30
#define CPACR_ASEDIS_BIT             31

#define FPSID_SW_BIT                 23
#define FPSID_SUBARCH_SHIFT_POS      16

static void clearEnFPEXC(void)
{
    word_t fpexc;
    VMRS(FPEXC, fpexc);
    fpexc &= ~BIT(FPEXC_EN_BIT);
    VMSR(FPEXC, fpexc);
}

#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined(CONFIG_HAVE_FPU)

#define HCPTR_CP10_BIT  10
#define HCPTR_CP11_BIT  11
#define HCPTR_TASE_BIT  15
#define HCPTR_MASK      ~(BIT(HCPTR_CP10_BIT) | BIT(HCPTR_CP11_BIT) | BIT(HCPTR_TASE_BIT))

/* enable FPU accesses in Hyp mode */
static inline void enableFpuInstInHyp(void)
{
    if (!ARCH_NODE_STATE(armHSFPUEnabled)) {
        setHCPTR(getHCPTR() & HCPTR_MASK);
        ARCH_NODE_STATE(armHSFPUEnabled) = true;
    }
}

/* trap PL0/PL1 FPU operations to Hyp mode and disable FPU accesses in Hyp */
static inline void trapFpuInstToHyp(void)
{
    if (ARCH_NODE_STATE(armHSFPUEnabled)) {
        setHCPTR(getHCPTR() | ~HCPTR_MASK);
        ARCH_NODE_STATE(armHSFPUEnabled) = false;
    }
}

#else

static inline void enableFpuInstInHyp(void) {}
static inline void trapFpuInstToHyp(void) {}

#endif
#ifdef CONFIG_HAVE_FPU

/* This variable is set at init time to true if the FPU supports 32 registers (d0-d31) */
extern bool_t isFPUD32SupportedCached;

static void setEnFPEXC(void)
{
    word_t fpexc;
    VMRS(FPEXC, fpexc);
    fpexc |=  BIT(FPEXC_EN_BIT);
    VMSR(FPEXC, fpexc);
}
/* Store state in the FPU registers into memory. */
static inline void saveFpuState(tcb_t *thread)
{
    user_fpu_state_t *dest = &thread->tcbArch.tcbContext.fpuState;
    word_t fpexc;

    /* Fetch FPEXC. */
    VMRS(FPEXC, fpexc);

    dest->fpexc = fpexc;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* before touching the registers, we need to set the EN bit */
        setEnFPEXC();
    }

    if (isFPUD32SupportedCached) {
        register word_t regs_d16_d31 asm("ip") = (word_t) &dest->fpregs[16];
        asm volatile(
            ".word 0xeccc0b20        \n"    /*  vstmia  ip, {d16-d31} */
            :
            : "r"(regs_d16_d31)
            : "memory"
        );
    }

    register word_t regs_d0_d15 asm("r2") = (word_t) &dest->fpregs[0];
    asm volatile(
        /* Store d0 - d15 to memory */
        ".word 0xec820b20       \n" /* vstmia  r2, {d0-d15}" */
        :
        : "r"(regs_d0_d15)
    );

    /* Store FPSCR. */
    VMRS(FPSCR, dest->fpscr);

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* Restore the FPEXC. */
        VMSR(FPEXC, fpexc);
    }
}

/* Enable the FPU to be used without faulting.
 * Required even if the kernel attempts to use the FPU.
 *
 * The FPEXC_EN bit is not set immediately in HYP mode for
 * the following reason:
 *
 *    A VM can set/clear the EN bit in the FPEXC in order to
 *    trap FPU accesses, implementing its own save/restore
 *    functions. Thus, we need to save the FPEXC without modifying
 *    it.
 *
 */
/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void enableFpu(void)
{
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    enableFpuInstInHyp();
    if (!ARCH_NODE_STATE(armHSVCPUActive)) {
        setEnFPEXC();
    }
#else
    setEnFPEXC();
#endif
}

/* Load FPU state from memory into the FPU registers. */
static inline void loadFpuState(const tcb_t *thread)
{
    const user_fpu_state_t *src = &thread->tcbArch.tcbContext.fpuState;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* now we need to enable the EN bit in FPEXC */
        setEnFPEXC();
    }
    register word_t regs_d16_d31 asm("r2") = (word_t) &src->fpregs[16];
    if (isFPUD32SupportedCached) {
        asm volatile(
            ".word 0xecd20b20       \n" /*   vldmia  r2, {d16-d31} */
            :: "r"(regs_d16_d31)
        );
    }

    register word_t regs_d0_d15 asm("r0") = (word_t) &src->fpregs[0];
    asm volatile(
        /* Restore d0 - d15 from memory */
        ".word 0xec900b20         \n"    /*  vldmia  r0, {d0-d15} */
        :: "r"(regs_d0_d15)
    );

    /* Load FPSCR. */
    VMSR(FPSCR, src->fpscr);

    /* Restore FPEXC. */
    VMSR(FPEXC, src->fpexc);
}

#endif /* CONFIG_HAVE_FPU */


/* Disable the FPU so that usage of it causes a fault.
 * In HYP mode, when a native thread is running:
 *     if the EN FPEXC is set, trapFpuHyp causes ensures a trap to HYP mode;
 *     if the EN is off, an undefined instruction exception is triggered.
 *
 * Either way, the kernel gets back in control.
 * When a VM is running, always, a trap to HYP mode is triggered.
 * Thus, we do not need to modify the EN bit of the FPEXC.
 */
static inline void disableFpu(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        trapFpuInstToHyp();
    } else {
        clearEnFPEXC();
    }
}

