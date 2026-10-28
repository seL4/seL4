/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

#include <arch/object/vcpu.h>
#include <drivers/timer/arm_generic.h>
#include <arch/machine/debug.h>

/* HCR_EL2 values common to vCPUs and native tasks.
 * Audited against ARM ARM DDI0487M.d, D24-8919.
 *
 * Set at 1:
 * - VM, virtualization enable
 * - FMO|IMO|AMO, force on CPSR.AIF
 * - TSC, trap SMC calls to EL2
 * - TIDCP, trap IMPDEF sysreg access - needed as we cannot context switch them
 * - TACR, trap ACTLR_EL1 access - needed as we should not context switch CPU
 *   IMPDEF 'chicken bits'.
 * - RW, controls lower-EL AArch64 vs AArch32 execution state with FEAT_AA32EL1,
 *   or RAO/WI otherwise.
 * - TLOR, traps accesses to LOR registers with FEAT_LOR - we do not context
 *   switch these registers, so we must trap them. (RES0 otherwise)
 * - TERR, traps accesses to RAS registers - we do not context switch these
 *   registers (RES0 otherwise).
 * - TID5, traps access to GMID_EL1 with MTE2 - current fields are RO, but the
 *   whole register is not RO, so newer ARM versions could make RW. Trap just
 *   in case (we also don't support MTE2).
 * - SWIO, set/way invalidation override - redundant on ARMv8-A, where VM=1
 *   already has this effect, but set for uniformity with ARMv7-A. Irrelevant
 *   for native tasks, because set/way instructions are already UNDEFINED at EL0.
 *
 * Set at 0:
 * - PTW, protected table walk - unneeded
 * - VF/VI/VA, virtual FIRQ/IRQ/async pending - unneeded
 * - FB, force broadcast - unneeded
 * - BSU, barrier shareability upgrade - unneeded
 * - TID0/TID1/TID2/TID3/TID4, ID register traps - accesses to registers either RO or
 *   context switched by seL4.
 * - TSW/TPCP/TPU, traps cache maintenance ops by set/way, to PoC,PoP, and PoU -
 *   unneeded as these will all be clean/clean&invalidate which cannot corrupt
 *   data; and trapping these would not prevent cache timing channels.
 * - TDZ, traps DC ZVA operations - equivalent to zeroing memory.
 * - HCD, traps HVC execution - unneeded, only matters without EL3, is undef
 *   at EL0 always, and EL1 would trap to EL2 anyway.
 * - CD/ID, forces non-cacheable instruction and data cache EL1&0/stage-2
 *   translations - unneeded
 * - E2H, "EL2 Host" for VHE - seL4 implements the Armv8.0 non-VHE hyp extensions
 *   so we set to 0 to disable. RES0 when hardware does not implement.
 * - TEA, route synchronous external aborts to EL2 (FEAT_RAS), or RES0 - seL4
 *   does not support the RAS extensions and it should be fine for these to
 *   be trapped at EL1, as they are synchronous and VMs can handle them.
 * - APK, trap Pointer authentication key registers (or RES0) - 0 enables the
 *   traps, which is safe behaviour.
 * - API, trap pointer authentication instructions (or RES0) - 0 enables the
 *   traps, which is safe behaviour.
 * - NV/NV1/AT/NV2, nested virtualisation features (or RES0) - seL4 does not
 *   support nested virtualisation.
 * - FWB, forced writeback (or RES0) - changes how memory/cacheability attrs
 *   are combined in stage 2 translations. not necessary
 * - FIEN, fault injection enable for RAS (or RES0) - 0 enables the traps, which
 *   is the safe behaviour.
 * - GPF, granule protection faults with RME (or RES0) - 0 allows these to
 *   route to EL1
 * - TICAB, traps instruction cache maintenance instructions (or RES0) - unneeded
 * - AMVOFFEN, activity monitor virtual offsets (or RES0) - unneeded, we set
 *   CPT_EL2.TAM to 0 to disable activity monitors.
 * - TOCU, traps cache maintenance ops to PoU (or RES0) - unneeded
 * - EnSCXT, software context number register access (or RES0) - 0 enables the
 *   traps, which is safe behaviour.
 * - TTLBIS/TTLBOS, traps TLB maintenance (or RES0) - unneeded
 * - ATA, traps memory tagging registers - 0 enables the traps, which is safe
 *   behaviour.
 * - DCT, controls 'tagged attribute' in stage 1 translations (MTE2) - unneeded,
 *   seL4 does not enable the MTE extensions.
 * - TWEDEn/TWEDEL, controls WFE instruction trap delays - unneeded, was added
 *   as a performant-relevant optimisation.
 * - FWB (forced write-back, Arm-v8.3+) changes PTE format, does not change
 *   isolation behavior, only changes cache maintanance.
 * - GPF (Armv9+), not needed. EL0 is already routed to EL2 because of TGE, EL1
 *   can go to EL1 to be handled there.
 *
 *
 * Varies:
 * - DC, default cacheable - contrary to name, this makes CPU act as if
 *   SCTLR_EL1.M is 0, i.e. EL1&0 stage 1 address translation disabled. Set for
 *   native tasks, but not for vCPUs.
 * - TWI/TWE, trap WFI/WFE at EL0 and EL1. Set for vCPUs depending on
 *   CONFIG_DISABLE_WFI_WFE_TRAPS. Not set for native tasks. SCTLR_EL1_NATIVE
 *   sets nTWI/nTWE, so EL0 WFI/WFE execute and the core wakes on the next
 *   interrupt (e.g. the kernel timer). Only affects the user-level thread
 *   itself.
 * - TTLB, trap TLB operations - unneeded (0) for vCPUs @ EL1, conservatively
 *   trapped for EL0 native tasks (even though they are UNDEF).
 * - TVM/TRVM, traps writes/reads to virtual memory sysregs - for vCPUs @ EL1,
 *   unneeded (0) and would be slow if set; conservatively trapped for EL0
 *   native tasks (even though UNDEF).
 * - TGE, trap EL1 general exceptions (to EL2) - used (1) for EL0 native tasks
 *   to trap exceptions to seL4, not used EL1, not used for vCPUs.
 */

#ifdef CONFIG_DISABLE_WFI_WFE_TRAPS
#define HCR_MAYBE_TWI_TWE ( 0 )
#else
#define HCR_MAYBE_TWI_TWE ( HCR_TWI | HCR_TWE )
#endif

#define HCR_COMMON ( HCR_VM | HCR_AMO | HCR_IMO | HCR_FMO | HCR_TSC | HCR_TIDCP \
                   | HCR_TACR | HCR_SWIO | HCR_RW | HCR_TLOR | HCR_TERR | HCR_TID5)

#define HCR_NATIVE ( HCR_COMMON | HCR_DC | HCR_TTLB | HCR_TVM | HCR_TRVM | HCR_TGE )

#define HCR_VCPU   ( HCR_COMMON | HCR_MAYBE_TWI_TWE )

/* CPTR_EL2 value set at boot. EL0 and EL1 accesses to the following trap to EL2
 * unless enabled explicitly later.
 *
 * CPTR_EL2 fields for E2H0 variant layout (HCR_EL2.E2H == 0):
 *
 * - TFP: trap FP and Advanced SIMD. disableTrapFpu() enables FPU later when
 *   configured.
 * - TZ, TSM: trap SVE and SME. The kernel does not context switch this state.
 *   Both bits are RES1 when the feature is not implemented.
 * - TTA: trap trace register accesses. These are not context switched.
 * - TCPAC: not set, CPACR_EL1 is context-switched per VCPU.
 * - TAM: trap access to the activity monitor registers. RES0 when the feature
 *   is not present, but writing 1 cannot affect operation in that case.
 * - All other bits are RES0 or part of CPTR_EL2_RES1.
 * - E0POE, SMEN, FPEN, ZEN: exist only in the E2H1 variant.
 */
#define CPTR_EL2_DEFAULT    (CPTR_EL2_RES1 | BIT(CPTR_EL2_TTA) | BIT(CPTR_EL2_TFP) | BIT(CPTR_EL2_TAM))

#define SCTLR_EL1_UCI       BIT(26)     /* Enable EL0 access to DC CVAU, DC CIVAC, DC CVAC,
                                           and IC IVAU in AArch64 state   */
#define SCTLR_EL1_C         BIT(2)      /* Enable data and unified caches */
#define SCTLR_EL1_I         BIT(12)     /* Enable instruction cache       */
#define SCTLR_EL1_CP15BEN   BIT(5)      /* AArch32 CP15 barrier enable    */
#define SCTLR_EL1_UCT       BIT(15)     /* Enable EL0 access to CTR_EL0   */
#define SCTLR_EL1_NTWI      BIT(16)     /* WFI executed as normal         */
#define SCTLR_EL1_NTWE      BIT(18)     /* WFE executed as normal         */

#ifdef CONFIG_AARCH64_USER_CACHE_ENABLE
#define SCTLR_NATIVE_USER_CACHE_OPS (SCTLR_EL1_UCI | SCTLR_EL1_UCT)
#else
#define SCTLR_NATIVE_USER_CACHE_OPS
#endif

/* Disable MMU, SP alignment check, and alignment check */
/* A57 default value */
#define SCTLR_EL1_RES      0x30d00800   /* Reserved value */
#define SCTLR_EL1          ( SCTLR_EL1_RES | SCTLR_EL1_CP15BEN | \
                             SCTLR_EL1_NTWI | SCTLR_EL1_NTWE )
#define SCTLR_EL1_NATIVE   (SCTLR_EL1 | SCTLR_EL1_C | SCTLR_EL1_I | SCTLR_NATIVE_USER_CACHE_OPS)
#define SCTLR_EL1_VM       (SCTLR_EL1 | SCTLR_EL1_UCT | SCTLR_EL1_UCI)
#define SCTLR_DEFAULT      SCTLR_EL1_NATIVE

/* TCR_EL1 for native threads. Stage 1 translation is off, but fields like
 * TBI0/TBI1 still apply to EL0. 0 is a safe EL0 value for all relevant fields.
 */
#define TCR_EL1_NATIVE     0

#define UNKNOWN_FAULT       0x2000000
#define ESR_EC_TFP          0x7         /* Trap instructions that access FPU registers */
#define ESR_EC_CPACR        0x18        /* Trap access to CPACR                        */
#define ESR_EC(x)           (((x) & 0xfc000000) >> 26)

#define VTCR_EL2_T0SZ(x)    ((x) & 0x3f)
#define VTCR_EL2_SL0(x)     (((x) & 0x3) << 6)
#define VTCR_EL2_IRGN0(x)   (((x) & 0x3) << 8)
#define VTCR_EL2_ORGN0(x)   (((x) & 0x3) << 10)
#define VTCR_EL2_SH0(x)     (((x) & 0x3) << 12)
#define VTCR_EL2_TG0(x)     (((x) & 0x3) << 14)
#define VTCR_EL2_PS(x)      (((x) & 0x7) << 16)

/* Physical address size */
#define PS_4G               0
#define PS_64G              1
#define PS_1T               2
#define PS_4T               3
#define PS_16T              4
#define PS_256T             5

/* Translation granule size */
#define TG0_4K              0
#define TG0_64K             1
#define TG0_16K             2

#define ID_AA64MMFR0_EL1_PARANGE(x) ((x) & 0xf)
#define ID_AA64MMFR0_TGRAN4(x)      (((x) >> 28u) & 0xf)

/* Shareability attributes */
#define SH0_NONE            0
#define SH0_OUTER           2
#define SH0_INNER           3

/* Cacheability attributes */
#define NORMAL_NON_CACHEABLE    0
#define NORMAL_WB_WA_CACHEABLE  1 /* write-back, write-allocate      */
#define NORMAL_WT_CACHEABLE     2 /* write-through                   */
#define NORMAL_WB_NWA_CACHEABLE 3 /* write-back, no write-allocate   */

/* Start level  */
#define SL0_4K_L2       0         /* 4K, start at level 2 */
#define SL0_4K_L1       1         /* 4K, start at level 1 */
#define SL0_4K_L0       2         /* 4K, start at level 0 */

#define REG_SCTLR_EL1       "sctlr_el1"
#define REG_TTBR0_EL1       "ttbr0_el1"
#define REG_TTBR1_EL1       "ttbr1_el1"
#define REG_TCR_EL1         "tcr_el1"
#define REG_MAIR_EL1        "mair_el1"
#define REG_AMAIR_EL1       "amair_el1"
#define REG_CONTEXTIDR_EL1  "contextidr_el1"
#define REG_AFSR0_EL1       "afsr0_el1"
#define REG_AFSR1_EL1       "afsr1_el1"
#define REG_ESR_EL1         "esr_el1"
#define REG_FAR_EL1         "far_el1"
#define REG_VBAR_EL1        "vbar_el1"
#define REG_TPIDR_EL1       "tpidr_el1"
#define REG_SP_EL1          "sp_el1"
#define REG_ELR_EL1         "elr_el1"
#define REG_SPSR_EL1        "spsr_el1"
#define REG_CPACR_EL1       "cpacr_el1"
#define REG_PAR_EL1         "par_el1"
#define REG_CNTV_TVAL_EL0   "cntv_tval_el0"
#define REG_CNTV_CTL_EL0    "cntv_ctl_el0"
#define REG_CNTV_CVAL_EL0   "cntv_cval_el0"
#define REG_CNTVOFF_EL2     "cntvoff_el2"
#define REG_CNTKCTL_EL1     "cntkctl_el1"
#define REG_HCR_EL2         "hcr_el2"
#define REG_CPTR_EL2        "cptr_el2"
#define REG_VTCR_EL2        "vtcr_el2"
#define REG_VMPIDR_EL2      "vmpidr_el2"
#define REG_CSSELR_EL1      "csselr_el1"
/* Use generic encoding to avoid needing FEAT_RAS assembler feature */
#define REG_VDISR_EL2       "s3_4_c12_c1_1"
#define REG_ID_AA64PFR0_EL1 "id_aa64pfr0_el1"
#define REG_MPIDR_EL1       "mpidr_el1"
#define REG_ID_AA64MMFR0_EL1 "id_aa64mmfr0_el1"

/* for EL1 SCTLR */
static inline word_t getSCTLR(void)
{
    return readSystemControlRegister();
}

static inline void setSCTLR(word_t sctlr)
{
    writeSystemControlRegister(sctlr);
}

static inline word_t readTTBR0(void)
{
    word_t reg;
    MRS(REG_TTBR0_EL1, reg);
    return reg;
}

static inline void writeTTBR0(word_t reg)
{
    MSR(REG_TTBR0_EL1, reg);
}

static inline word_t readTTBR1(void)
{
    word_t reg;
    MRS(REG_TTBR1_EL1, reg);
    return reg;
}

static inline void writeTTBR1(word_t reg)
{
    MSR(REG_TTBR1_EL1, reg);
}

static inline word_t readTCR(void)
{
    word_t reg;
    MRS(REG_TCR_EL1, reg);
    return reg;
}

static inline void writeTCR(word_t reg)
{
    MSR(REG_TCR_EL1, reg);
}

static inline word_t readMAIR(void)
{
    word_t reg;
    MRS(REG_MAIR_EL1, reg);
    return reg;
}

static inline void writeMAIR(word_t reg)
{
    MSR(REG_MAIR_EL1, reg);
}

static inline word_t readAMAIR(void)
{
    word_t reg;
    MRS(REG_AMAIR_EL1, reg);
    return reg;
}

static inline void writeAMAIR(word_t reg)
{
    MSR(REG_AMAIR_EL1, reg);
}

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t readCIDR(void)
{
    uint32_t reg;
    MRS(REG_CONTEXTIDR_EL1, reg);
    return (word_t)reg;
}

/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void writeCIDR(word_t reg)
{
    MSR(REG_CONTEXTIDR_EL1, (uint32_t)reg);
}

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t readAFSR0(void)
{
    uint32_t reg;
    MRS(REG_AFSR0_EL1, reg);
    return (word_t)reg;
}

/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void writeAFSR0(word_t reg)
{
    MSR(REG_AFSR0_EL1, (uint32_t)reg);
}

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t readAFSR1(void)
{
    uint32_t reg;
    MRS(REG_AFSR1_EL1, reg);
    return (word_t)reg;
}

/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void writeAFSR1(word_t reg)
{
    MSR(REG_AFSR1_EL1, (uint32_t)reg);
}

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t readESR(void)
{
    uint32_t reg;
    MRS(REG_ESR_EL1, reg);
    return (word_t)reg;
}

/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline void writeESR(word_t reg)
{
    MSR(REG_ESR_EL1, (uint32_t)reg);
}

static inline word_t readFAR(void)
{
    word_t reg;
    MRS(REG_FAR_EL1, reg);
    return reg;
}

static inline void writeFAR(word_t reg)
{
    MSR(REG_FAR_EL1, reg);
}

static inline word_t readVBAR(void)
{
    word_t reg;
    MRS(REG_VBAR_EL1, reg);
    return reg;
}

static inline void writeVBAR(word_t reg)
{
    MSR(REG_VBAR_EL1, reg);
}

static inline word_t readSP_EL1(void)
{
    word_t reg;
    MRS(REG_SP_EL1, reg);
    return reg;
}

static inline void writeSP_EL1(word_t reg)
{
    MSR(REG_SP_EL1, reg);
}

static inline word_t readELR_EL1(void)
{
    word_t reg;
    MRS(REG_ELR_EL1, reg);
    return reg;
}

static inline void writeELR_EL1(word_t reg)
{
    MSR(REG_ELR_EL1, reg);
}

static inline word_t readSPSR_EL1(void)
{
    word_t reg;
    MRS(REG_SPSR_EL1, reg);
    return reg;
}

static inline void writeSPSR_EL1(word_t reg)
{
    MSR(REG_SPSR_EL1, reg);
}

static inline word_t readCPACR_EL1(void)
{
    word_t reg;
    MRS(REG_CPACR_EL1, reg);
    return reg;
}

static inline void writeCPACR_EL1(word_t reg)
{
    MSR(REG_CPACR_EL1, reg);
}

static inline word_t readPAR_EL1(void)
{
    word_t reg;
    MRS(REG_PAR_EL1, reg);
    return reg;
}

static inline void writePAR_EL1(word_t reg)
{
    MSR(REG_PAR_EL1, reg);
}

static inline word_t readCNTV_TVAL_EL0(void)
{
    word_t reg;
    MRS(REG_CNTV_TVAL_EL0, reg);
    return reg;
}

static inline void writeCNTV_TVAL_EL0(word_t reg)
{
    MSR(REG_CNTV_TVAL_EL0, reg);
}

static inline word_t readCNTV_CTL_EL0(void)
{
    word_t reg;
    MRS(REG_CNTV_CTL_EL0, reg);
    return reg;
}

static inline void writeCNTV_CTL_EL0(word_t reg)
{
    MSR(REG_CNTV_CTL_EL0, reg);
}

static inline word_t readCNTV_CVAL_EL0(void)
{
    word_t reg;
    MRS(REG_CNTV_CVAL_EL0, reg);
    return reg;
}

static inline void writeCNTV_CVAL_EL0(word_t reg)
{
    MSR(REG_CNTV_CVAL_EL0, reg);
}

static inline word_t readCNTVOFF_EL2(void)
{
    word_t reg;
    MRS(REG_CNTVOFF_EL2, reg);
    return reg;
}

static inline void writeCNTVOFF_EL2(word_t reg)
{
    MSR(REG_CNTVOFF_EL2, reg);
}

static inline word_t readCNTKCTL_EL1(void)
{
    word_t reg;
    MRS(REG_CNTKCTL_EL1, reg);
    return reg;
}

static inline void writeCNTKCTL_EL1(word_t reg)
{
    MSR(REG_CNTKCTL_EL1, reg);
}

static inline word_t readVMPIDR_EL2(void)
{
    word_t reg;
    MRS(REG_VMPIDR_EL2, reg);
    return reg;
}

static inline void writeVMPIDR_EL2(word_t reg)
{
    MSR(REG_VMPIDR_EL2, reg);
}

static inline word_t readCSSELR_EL1(void)
{
    word_t reg;
    MRS(REG_CSSELR_EL1, reg);
    return reg;
}

static inline void writeCSSELR_EL1(word_t reg)
{
    MSR(REG_CSSELR_EL1, reg);
}

static inline bool_t cpu_has_ras(void)
{
    word_t reg;
    MRS(REG_ID_AA64PFR0_EL1, reg);
    return ((reg >> ID_AA64PFR0_EL1_RAS) & MASK(4)) != 0;
}

/* VDISR_EL2 is undefined when FEAT_RAS is not supported. In this case, return 0 for read. */
static inline word_t readVDISR_EL2(void)
{
    word_t reg = 0;
    if (cpu_has_ras()) {
        MRS(REG_VDISR_EL2, reg);
    }
    return reg;
}

/* VDISR_EL2 is undefined when FEAT_RAS is not supported. In this case, ignore writes. */
static inline void writeVDISR_EL2(word_t reg)
{
    if (cpu_has_ras()) {
        MSR(REG_VDISR_EL2, reg);
    }
}

static inline void setHCR(word_t reg)
{
    MSR(REG_HCR_EL2, reg);
}

static word_t vcpu_hw_read_reg(word_t reg_index)
{
    word_t reg = 0;
    switch (reg_index) {
    case seL4_VCPUReg_SCTLR:
        return getSCTLR();
    case seL4_VCPUReg_TTBR0:
        return readTTBR0();
    case seL4_VCPUReg_TTBR1:
        return readTTBR1();
    case seL4_VCPUReg_TCR:
        return readTCR();
    case seL4_VCPUReg_MAIR:
        return readMAIR();
    case seL4_VCPUReg_AMAIR:
        return readAMAIR();
    case seL4_VCPUReg_CIDR:
        return readCIDR();
    case seL4_VCPUReg_CPACR:
        return readCPACR_EL1();
    case seL4_VCPUReg_PAR:
        return readPAR_EL1();
    case seL4_VCPUReg_AFSR0:
        return readAFSR0();
    case seL4_VCPUReg_AFSR1:
        return readAFSR1();
    case seL4_VCPUReg_ESR:
        return readESR();
    case seL4_VCPUReg_FAR:
        return readFAR();
    case seL4_VCPUReg_VBAR:
        return readVBAR();
    case seL4_VCPUReg_TPIDR_EL1:
        return readTPIDR_EL1();
    case seL4_VCPUReg_SP_EL1:
        return readSP_EL1();
    case seL4_VCPUReg_ELR_EL1:
        return readELR_EL1();
    case seL4_VCPUReg_SPSR_EL1:
        return readSPSR_EL1();
    case seL4_VCPUReg_CSSELR_EL1:
        return readCSSELR_EL1();
    case seL4_VCPUReg_VDISR_EL2:
        return readVDISR_EL2();
    case seL4_VCPUReg_CNTV_CTL:
        return readCNTV_CTL_EL0();
    case seL4_VCPUReg_CNTV_CVAL:
        return readCNTV_CVAL_EL0();
    case seL4_VCPUReg_CNTVOFF:
        return readCNTVOFF_EL2();
    case seL4_VCPUReg_CNTKCTL_EL1:
        return readCNTKCTL_EL1();
    case seL4_VCPUReg_VMPIDR_EL2:
        return readVMPIDR_EL2();
    default:
        fail("ARM/HYP: Invalid register index");
    }

    return reg;
}

static void vcpu_hw_write_reg(word_t reg_index, word_t reg)
{
    switch (reg_index) {
    case seL4_VCPUReg_SCTLR:
        setSCTLR(reg);
        break;
    case seL4_VCPUReg_TTBR0:
        writeTTBR0(reg);
        break;
    case seL4_VCPUReg_TTBR1:
        writeTTBR1(reg);
        break;
    case seL4_VCPUReg_TCR:
        writeTCR(reg);
        break;
    case seL4_VCPUReg_MAIR:
        writeMAIR(reg);
        break;
    case seL4_VCPUReg_AMAIR:
        writeAMAIR(reg);
        break;
    case seL4_VCPUReg_CIDR:
        writeCIDR(reg);
        break;
    case seL4_VCPUReg_CPACR:
        writeCPACR_EL1(reg);
        break;
    case seL4_VCPUReg_PAR:
        writePAR_EL1(reg);
        break;
    case seL4_VCPUReg_AFSR0:
        writeAFSR0(reg);
        break;
    case seL4_VCPUReg_AFSR1:
        writeAFSR1(reg);
        break;
    case seL4_VCPUReg_ESR:
        writeESR(reg);
        break;
    case seL4_VCPUReg_FAR:
        writeFAR(reg);
        break;
    case seL4_VCPUReg_VBAR:
        writeVBAR(reg);
        break;
    case seL4_VCPUReg_TPIDR_EL1:
        writeTPIDR_EL1(reg);
        break;
    case seL4_VCPUReg_SP_EL1:
        writeSP_EL1(reg);
        break;
    case seL4_VCPUReg_ELR_EL1:
        writeELR_EL1(reg);
        break;
    case seL4_VCPUReg_SPSR_EL1:
        writeSPSR_EL1(reg);
        break;
    case seL4_VCPUReg_CSSELR_EL1:
        writeCSSELR_EL1(reg);
        break;
    case seL4_VCPUReg_VDISR_EL2:
        writeVDISR_EL2(reg);
        break;
    case seL4_VCPUReg_CNTV_CTL:
        writeCNTV_CTL_EL0(reg);
        break;
    case seL4_VCPUReg_CNTV_CVAL:
        writeCNTV_CVAL_EL0(reg);
        break;
    case seL4_VCPUReg_CNTVOFF:
        writeCNTVOFF_EL2(reg);
        break;
    case seL4_VCPUReg_CNTKCTL_EL1:
        writeCNTKCTL_EL1(reg);
        break;
    case seL4_VCPUReg_VMPIDR_EL2:
        writeVMPIDR_EL2(reg);
        break;
    default:
        fail("ARM/HYP: Invalid register index");
    }
}

/** DONT_TRANSLATE */
static inline void vcpu_init_vtcr(void)
{

    /* check that the processor supports the configuration */
    uint32_t val;
    MRS(REG_ID_AA64MMFR0_EL1, val);
    uint32_t pa_range = ID_AA64MMFR0_EL1_PARANGE(val);
    if (config_set(CONFIG_ARM_PA_SIZE_BITS_40) && pa_range < PS_1T) {
        fail("Processor does not support a 40 bit PA");
    }
    if (config_set(CONFIG_ARM_PA_SIZE_BITS_44) && pa_range < PS_16T) {
        fail("Processor does not support a 44 bit PA");
    }
    uint32_t granule = ID_AA64MMFR0_TGRAN4(val);
    if (granule) {
        fail("Processor does not support 4KB");
    }

    /* Set up the stage-2 translation control register for cores supporting 44-bit PA */
    uint32_t vtcr_el2;
#ifdef CONFIG_ARM_PA_SIZE_BITS_40
    vtcr_el2 = VTCR_EL2_T0SZ(24);                            // 40-bit input IPA
    vtcr_el2 |= VTCR_EL2_PS(PS_1T);                          // 40-bit PA size
    vtcr_el2 |= VTCR_EL2_SL0(SL0_4K_L1);                     // 4KiB, start at level 1
#else
    vtcr_el2 = VTCR_EL2_T0SZ(20);                            // 44-bit input IPA
    vtcr_el2 |= VTCR_EL2_PS(PS_16T);                         // 44-bit PA size
    vtcr_el2 |= VTCR_EL2_SL0(SL0_4K_L0);                     // 4KiB, start at level 0
#endif
    vtcr_el2 |= VTCR_EL2_IRGN0(NORMAL_WB_WA_CACHEABLE);      // inner write-back, read/write allocate
    vtcr_el2 |= VTCR_EL2_ORGN0(NORMAL_WB_WA_CACHEABLE);      // outer write-back, read/write allocate
    vtcr_el2 |= VTCR_EL2_SH0(SH0_INNER);                     // inner shareable
    vtcr_el2 |= VTCR_EL2_TG0(TG0_4K);                        // 4KiB page size
    vtcr_el2 |= BIT(31);                                     // reserved as 1

    MSR(REG_VTCR_EL2, vtcr_el2);
    isb();
}

static inline void armv_vcpu_boot_init(void)
{
    word_t hcr_el2 = 0;

    vcpu_init_vtcr();

    hcr_el2 = HCR_NATIVE;
    MSR(REG_HCR_EL2, hcr_el2);
    isb();

    /* Trap EL0 and EL1 FPU, SVE, SME and trace accesses to EL2. FPU init later
     * in init_cpu() enables FPU as needed. */
    MSR(REG_CPTR_EL2, CPTR_EL2_DEFAULT);
    /* isb() below is enough, we are not making a change that is relevant to EL2 access */

    /* set the SCTLR_EL1 and TCR_EL1 for running native seL4 threads */
    MSR(REG_SCTLR_EL1, SCTLR_EL1_NATIVE);
    MSR(REG_TCR_EL1, TCR_EL1_NATIVE);
    isb();

    initHDCR();
}

static inline void armv_vcpu_save(vcpu_t *vcpu, bool_t active)
{
    /* If we aren't active then this state already got stored when we were disabled */
    if (active) {
        vcpu_save_reg(vcpu, seL4_VCPUReg_CPACR);
        vcpu_save_reg(vcpu, seL4_VCPUReg_TCR);
    }
    vcpu_save_reg_range(vcpu, seL4_VCPURegSaveRange_start, seL4_VCPURegSaveRange_end);
}

static inline void vcpu_enable(vcpu_t *vcpu)
{
    vcpu_restore_reg(vcpu, seL4_VCPUReg_TCR);
    vcpu_restore_reg(vcpu, seL4_VCPUReg_SCTLR);
    setHCR(HCR_VCPU);
    isb();

    set_gic_vcpu_ctrl_hcr(vcpu->vgic.hcr);

    vcpu_restore_reg(vcpu, seL4_VCPUReg_CPACR);
    /* Restore virtual timer state */
    restore_virt_timer(vcpu);
}

static inline void vcpu_disable(vcpu_t *vcpu)
{

    uint32_t hcr;
    dsb();
    if (likely(vcpu)) {
        hcr = get_gic_vcpu_ctrl_hcr();
        vcpu->vgic.hcr = hcr;
        vcpu_save_reg(vcpu, seL4_VCPUReg_SCTLR);
        vcpu_save_reg(vcpu, seL4_VCPUReg_CPACR);
        vcpu_save_reg(vcpu, seL4_VCPUReg_TCR);
    }
    /* Turn off the VGIC */
    set_gic_vcpu_ctrl_hcr(0);
    /* for GIC memory-mapped register */
    dsb();

    /* Stage 1 MMU off */
    setSCTLR(SCTLR_DEFAULT);
    /* Guest TBI settings would otherwise apply to native threads */
    writeTCR(TCR_EL1_NATIVE);
    setHCR(HCR_NATIVE);
    isb();

    /* Allow FPU instructions in EL0 and EL1 for native
     * threads by setting the CPACR_EL1. The CPTR_EL2 is
     * used to trap the FPU instructions to EL2.
     */
    enableFpuEL01();
    if (likely(vcpu)) {
        /* Save virtual timer state */
        save_virt_timer(vcpu);
        /* Mask the virtual timer interrupt */
        maskInterrupt(true, CORE_IRQ_TO_IRQT(CURRENT_CPU_INDEX(), INTERRUPT_VTIMER_EVENT));
    }
}

static inline void armv_vcpu_init(vcpu_t *vcpu)
{
    vcpu_write_reg(vcpu, seL4_VCPUReg_SCTLR, SCTLR_EL1_VM);
}

static inline bool_t armv_handleVCPUFault(word_t hsr)
{
#ifdef CONFIG_HARDWARE_DEBUG_API
    if (isDebugFault(hsr)) {
        handleDebugFaultEvent(hsr);
        return true;
    }
#endif

    if (hsr == UNKNOWN_FAULT) {
        handleUserLevelFault(getESR(), 0);
        return true;
    }

    return false;
}

static inline bool_t vcpu_reg_saved_when_disabled(word_t field)
{
    switch (field) {
    case seL4_VCPUReg_SCTLR:
    case seL4_VCPUReg_CNTV_CTL:
    case seL4_VCPUReg_CNTV_CVAL:
    case seL4_VCPUReg_CNTVOFF:
    case seL4_VCPUReg_CNTKCTL_EL1:
    case seL4_VCPUReg_CPACR:
    case seL4_VCPUReg_TCR:
        return true;
    default:
        return false;
    }
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */


