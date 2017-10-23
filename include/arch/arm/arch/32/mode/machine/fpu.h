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

#ifndef __MODE_MACHINE_FPU_H
#define __MODE_MACHINE_FPU_H

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

#define FPEXC_EX_BIT                 31
#define FPEXC_EN_BIT                 30

#if defined(CONFIG_ARM_CORTEX_A7) || defined(CONFIG_ARM_CORTEX_A9)
#define FPEXC_DEX_BIT                29
#endif

#define FPEXC_DEX_BIT                29
#define FPEXC_FP2V_BIT               28

extern bool_t isFPUEnabledCached[CONFIG_MAX_NUM_NODES];

static void clearEnFPEXC(void)
{
    word_t fpexc;
    MRC(FPEXC, fpexc);
    fpexc &= ~BIT(FPEXC_EN_BIT);
    MCR(FPEXC, fpexc);
}

#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined(CONFIG_HAVE_FPU)

#define HCPTR_CP10_BIT  10
#define HCPTR_CP11_BIT  11
#define HCPTR_TASE_BIT  15
#define HCPTR_MASK      ~(BIT(HCPTR_CP10_BIT) | BIT(HCPTR_CP11_BIT) | BIT(HCPTR_TASE_BIT))

/* enable FPU accesses in Hyp mode */
static inline void
enableFpuInstInHyp(void)
{
    if (!armHSFPUEnabled) {
        setHCPTR(getHCPTR() & HCPTR_MASK);
        armHSFPUEnabled = true;
    }
}

/* trap PL0/PL1 FPU operations to Hyp mode and disable FPU accesses in Hyp */
static inline void
trapFpuInstToHyp(void)
{
    if (armHSFPUEnabled) {
        setHCPTR(getHCPTR() | ~HCPTR_MASK);
        armHSFPUEnabled = false;
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
    MRC(FPEXC, fpexc);
    fpexc |=  BIT(FPEXC_EN_BIT);
    MCR(FPEXC, fpexc);
}
/* Store state in the FPU registers into memory. */
static inline void saveFpuState(user_fpu_state_t *dest)
{
    word_t fpexc;

    /* Store FPEXC */
    MRC(FPEXC, fpexc);

#if defined(CONFIG_ARM_CORTEX_A7) || defined(CONFIG_ARM_CORTEX_A9)
    /*
    * Reset DEX bit to 0 in case a subarchitecture sets it.
    * For example, Cortex-A7/A9 set this bit on deprecated vector VFP operations.
    */
    if (unlikely(fpexc & BIT(FPEXC_DEX_BIT))) {
        fpexc &= ~BIT(FPEXC_DEX_BIT);
        MCR(FPEXC, fpexc);
    }
#endif

    dest->fpexc = fpexc;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* before touching the regsiters, we need to set the EN bit */
        setEnFPEXC();
    }

    /* We don't support asynchronous exceptions */
    assert ((dest->fpexc & BIT(FPEXC_EX_BIT)) == 0);

    if (isFPUD32SupportedCached) {
        register word_t regs_d16_d31 asm("ip") = (word_t) &dest->fpregs[16];
        asm volatile(
            ".word 0xeccc0b20        \n"    /*  vstmia  ip, {d16-d31} */
            :
            : "r" (regs_d16_d31)
            : "memory"
        );
    }

    register word_t regs_d0_d15 asm("r2") =  (word_t) &dest->fpregs[0];
    asm volatile(
        /* Store d0 - d15 to memory */
        ".word 0xec820b20       \n" /* vstmia  r2, {d0-d15}" */
        /* Store PFSCR */
        ".word 0xeef1ea10       \n" /* vmrs   lr, fpscr */
        "str  lr, [%[tcb_fpscr]]\n"
        :
        : [tcb_fpscr] "r" (&dest->fpscr), "r" (regs_d0_d15)
        : "memory", "lr"
    );
    /* restore the FPEXC */
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        MCR(FPEXC, fpexc);
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

static inline void enableFpu(void)
{
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    enableFpuInstInHyp();
    if (!armHSVCPUActive) {
        setEnFPEXC();
    }
#else
    setEnFPEXC();
#endif
    isFPUEnabledCached[SMP_TERNARY(getCurrentCPUIndex(), 0)] = true;
}

/* Check if FPU is enable */
static inline bool_t isFpuEnable(void)
{
    return isFPUEnabledCached[SMP_TERNARY(getCurrentCPUIndex(), 0)];
}

/* Load FPU state from memory into the FPU registers. */
static inline void loadFpuState(user_fpu_state_t *src)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        /* now we need to enable the EN bit in FPEXC */
        setEnFPEXC();
    }
    register word_t regs_d16_d31 asm("r2") =  (word_t) &src->fpregs[16];
    if (isFPUD32SupportedCached) {
        asm volatile(
            ".word 0xecd20b20       \n" /*   vldmia  r2, {d16-d31} */
            :: "r" (regs_d16_d31)
        );
    }

    register word_t regs_d0_d15 asm("r0") =  (word_t) &src->fpregs[0];
    register word_t regs_fpscr asm("r1") = src->fpscr;
    asm volatile(
        /* Restore d0 - d15 from memory */
        ".word 0xec900b20         \n"    /*  vldmia  r0, {d0-d15} */
        /* Load fpscr */
        ".word 0xeee11a10         \n"    /*  vmsr    fpscr, r1 */
        :: "r" (regs_d0_d15), "r" (regs_fpscr)
    );

    /* Restore FPEXC */
    MCR(FPEXC, src->fpexc);
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
    isFPUEnabledCached[SMP_TERNARY(getCurrentCPUIndex(), 0)] = false;
}

#endif /* __MODE_MACHINE_FPU_H */
