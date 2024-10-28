/*
 * Copyright 2019, DornerWorks
 * Copyright 2019, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/*
 * Arm Generic Interrupt Controller v3
 */

#pragma once

/* tell the kernel we have the set trigger feature */
#define HAVE_SET_TRIGGER 1

#include <stdint.h>
#include <util.h>
#include <linker.h>
#include <mode/smp/smp.h>
#include <model/statedata.h>
#include <armv/machine.h>

#include "gic_common.h"

#define GIC_PRI_LOWEST     0xf0
#define GIC_PRI_IRQ        0xa0
#define GIC_PRI_HIGHEST    0x80 /* Higher priorities belong to Secure-World */

#define IRQ_MASK MASK(16u)
#define GIC_VCPU_MAX_NUM_LR 16

/* Register bits */
#define GICD_CTL_ENABLE 0x1
#define GICD_CTLR_RWP                BIT(31)
#define GICD_CTLR_ARE_NS             BIT(4)
#define GICD_CTLR_ENABLE_G1NS         BIT(1)
#define GICD_CTLR_ENABLE_G0          BIT(0)
#define GICD_IROUTER_SPI_MODE_ANY    BIT(31)

#define GICD_TYPE_LINESNR 0x01f

#define GICC_SRE_EL1_SRE             BIT(0)

#define GICR_WAKER_ProcessorSleep    BIT(1)
#define GICR_WAKER_ChildrenAsleep    BIT(2)

#define GICC_CTLR_EL1_EOImode_drop   BIT(1)

#define DEFAULT_PMR_VALUE            0xff

/* System registers for GIC CPU interface */
#ifdef CONFIG_ARCH_AARCH64
#define ICC_IAR1_EL1    "S3_0_C12_C12_0"
#define ICC_EOIR1_EL1   "S3_0_C12_C12_1"
#define ICC_HPPIR1_EL1  "S3_0_C12_C12_2"
#define ICC_BPR1_EL1    "S3_0_C12_C12_3"
#define ICC_DIR_EL1     "S3_0_C12_C11_1"
#define ICC_PMR_EL1     "S3_0_C4_C6_0"
#define ICC_CTLR_EL1    "S3_0_C12_C12_4"
#define ICC_IGRPEN1_EL1 "S3_0_C12_C12_7"
#define ICC_SRE_EL1     "S3_0_C12_C12_5"
#define MPIDR           "mpidr_el1"
/* Virt control registers */
#define ICH_AP0R0_EL2   "S3_4_C12_C8_0"
#define ICH_AP0R1_EL2   "S3_4_C12_C8_1"
#define ICH_AP0R2_EL2   "S3_4_C12_C8_2"
#define ICH_AP0R3_EL2   "S3_4_C12_C8_3"
#define ICH_AP1R0_EL2   "S3_4_C12_C9_0"
#define ICH_AP1R1_EL2   "S3_4_C12_C9_1"
#define ICH_AP1R2_EL2   "S3_4_C12_C9_2"
#define ICH_AP1R3_EL2   "S3_4_C12_C9_3"
#define ICH_HCR_EL2     "S3_4_C12_C11_0"
#define ICH_VTR_EL2     "S3_4_C12_C11_1"
#define ICH_MISR_EL2    "S3_4_C12_C11_2"
#define ICH_EISR_EL2    "S3_4_C12_C11_3"
#define ICH_ELRSR_EL2   "S3_4_C12_C11_5"
#define ICH_VMCR_EL2    "S3_4_C12_C11_7"
#define ICH_LR0_EL2     "S3_4_C12_C12_0"
#define ICH_LR1_EL2     "S3_4_C12_C12_1"
#define ICH_LR2_EL2     "S3_4_C12_C12_2"
#define ICH_LR3_EL2     "S3_4_C12_C12_3"
#define ICH_LR4_EL2     "S3_4_C12_C12_4"
#define ICH_LR5_EL2     "S3_4_C12_C12_5"
#define ICH_LR6_EL2     "S3_4_C12_C12_6"
#define ICH_LR7_EL2     "S3_4_C12_C12_7"
#define ICH_LR8_EL2     "S3_4_C12_C13_0"
#define ICH_LR9_EL2     "S3_4_C12_C13_1"
#define ICH_LR10_EL2    "S3_4_C12_C13_2"
#define ICH_LR11_EL2    "S3_4_C12_C13_3"
#define ICH_LR12_EL2    "S3_4_C12_C13_4"
#define ICH_LR13_EL2    "S3_4_C12_C13_5"
#define ICH_LR14_EL2    "S3_4_C12_C13_6"
#define ICH_LR15_EL2    "S3_4_C12_C13_7"
#else
#define ICC_IAR1_EL1    " p15, 0,  %0, c12,  c12, 0"
#define ICC_EOIR1_EL1   " p15, 0,  %0, c12,  c12, 1"
#define ICC_HPPIR1_EL1  " p15, 0,  %0, c12,  c12, 2"
#define ICC_BPR1_EL1    " p15, 0,  %0, c12,  c12, 3"
#define ICC_DIR_EL1     " p15, 0,  %0, c12,  c11, 1"
#define ICC_PMR_EL1     " p15, 0,  %0, c4,  c6, 0"
#define ICC_CTLR_EL1    " p15, 0,  %0, c12,  c12, 4"
#define ICC_IGRPEN1_EL1 " p15, 0,  %0, c12,  c12, 7"
#define ICC_SRE_EL1     " p15, 0,  %0, c12,  c12, 5"
#define MPIDR           " p15, 0,  %0, c0,  c0, 5"

/* Note: Virtualization control registers not currently defined for AARCH32 */

#endif

/* Helpers for VGIC */
#define VGIC_HCR_EOI_INVALID_COUNT(hcr) (((hcr) >> 27) & 0x1f)
#define VGIC_HCR_VGRP1DIE               (1U << 7)
#define VGIC_HCR_VGRP1EIE               (1U << 6)
#define VGIC_HCR_VGRP0DIE               (1U << 5)
#define VGIC_HCR_VGRP0EIE               (1U << 4)
#define VGIC_HCR_NPIE                   (1U << 3)
#define VGIC_HCR_LRENPIE                (1U << 2)
#define VGIC_HCR_UIE                    (1U << 1)
#define VGIC_HCR_EN                     (1U << 0)
#define VGIC_MISR_VGRP1D                VGIC_HCR_VGRP1DIE
#define VGIC_MISR_VGRP1E                VGIC_HCR_VGRP1EIE
#define VGIC_MISR_VGRP0D                VGIC_HCR_VGRP0DIE
#define VGIC_MISR_VGRP0E                VGIC_HCR_VGRP0EIE
#define VGIC_MISR_NP                    VGIC_HCR_NPIE
#define VGIC_MISR_LRENP                 VGIC_HCR_LRENPIE
#define VGIC_MISR_U                     VGIC_HCR_UIE
#define VGIC_MISR_EOI                   VGIC_HCR_EN
#define VGIC_VTR_NLISTREGS(vtr)         ((((vtr) >>  0) & 0x3f) + 1)
#define VGIC_VTR_NPRIOBITS(vtr)         ((((vtr) >> 29) & 0x07) + 1)
#define VGIC_VTR_NPREBITS(vtr)          ((((vtr) >> 26) & 0x07) + 1)

/* Memory map for GIC distributor */
struct gic_dist_map {
    uint32_t ctlr;                /* 0x0000 */
    uint32_t typer;               /* 0x0004 */
    uint32_t iidr;                /* 0x0008 */
    uint32_t res0;                /* 0x000C */
    uint32_t statusr;             /* 0x0010 */
    uint32_t res1[11];            /* [0x0014, 0x0040) */
    uint32_t setspi_nsr;          /* 0x0040 */
    uint32_t res2;                /* 0x0044 */
    uint32_t clrspi_nsr;          /* 0x0048 */
    uint32_t res3;                /* 0x004C */
    uint32_t setspi_sr;           /* 0x0050 */
    uint32_t res4;                /* 0x0054 */
    uint32_t clrspi_sr;           /* 0x0058 */
    uint32_t res5[9];             /* [0x005C, 0x0080) */
    uint32_t igrouprn[32];        /* [0x0080, 0x0100) */

    uint32_t isenablern[32];        /* [0x100, 0x180) */
    uint32_t icenablern[32];        /* [0x180, 0x200) */
    uint32_t ispendrn[32];          /* [0x200, 0x280) */
    uint32_t icpendrn[32];          /* [0x280, 0x300) */
    uint32_t isactivern[32];        /* [0x300, 0x380) */
    uint32_t icactivern[32];        /* [0x380, 0x400) */

    uint32_t ipriorityrn[255];      /* [0x400, 0x7FC) */
    uint32_t res6;                  /* 0x7FC */

    uint32_t itargetsrn[254];       /* [0x800, 0xBF8) */
    uint32_t res7[2];               /* 0xBF8 */

    uint32_t icfgrn[64];            /* [0xC00, 0xD00) */
    uint32_t igrpmodrn[64];         /* [0xD00, 0xE00) */
    uint32_t nsacrn[64];            /* [0xE00, 0xF00) */
    uint32_t sgir;                  /* 0xF00 */
    uint32_t res8[3];               /* [0xF04, 0xF10) */
    uint32_t cpendsgirn[4];         /* [0xF10, 0xF20) */
    uint32_t spendsgirn[4];         /* [0xF20, 0xF30) */
    uint32_t res9[5236];            /* [0x0F30, 0x6100) */

    uint64_t iroutern[960];         /* [0x6100, 0x7F00) irouter<n> to configure IRQs
                                     * with INTID from 32 to 1019. iroutern[0] is the
                                     * interrupt routing for SPI 32 */
};

/* __builtin_offsetof is not in the verification C subset, so we can only check this in
   non-verification builds. We specifically do not declare a macro for the builtin, because
   we do not want break the verification subset by accident. */
unverified_compile_assert(error_in_gic_dist_map,
                          0x6100 == __builtin_offsetof(struct gic_dist_map, iroutern));

/* Memory map for GIC Redistributor Registers for control and physical LPI's */
struct gic_rdist_map {          /* Starting */
    uint32_t    ctlr;           /* 0x0000 */
    uint32_t    iidr;           /* 0x0004 */
    uint64_t    typer;          /* 0x0008 */
    uint32_t    statusr;        /* 0x0010 */
    uint32_t    waker;          /* 0x0014 */
    uint32_t    res0[10];       /* 0x0018 */
    uint64_t    setlpir;        /* 0x0040 */
    uint64_t    clrlpir;        /* 0x0048 */
    uint32_t    res1[8];        /* 0x0050 */
    uint64_t    propbaser;      /* 0x0070 */
    uint64_t    pendbaser;      /* 0x0078 */
    uint32_t    res2[8];        /* 0x0080 */
    uint64_t    invlpir;        /* 0x00a0 */
    uint32_t    res3[2];        /* 0x00a8 */
    uint64_t    invallr;        /* 0x00b0 */
    uint32_t    res4[2];        /* 0x00b8 */
    uint32_t    syncr;          /* 0x00c0 */
};

/* Memory map for the GIC Redistributor Registers for the SGI and PPI's */
struct gic_rdist_sgi_ppi_map {  /* Starting */
    uint32_t    res0[32];       /* 0x0000 */
    uint32_t    igroupr0;       /* 0x0080 */
    uint32_t    res1[31];       /* 0x0084 */
    uint32_t    isenabler0;     /* 0x0100 */
    uint32_t    res2[31];       /* 0x0104 */
    uint32_t    icenabler0;     /* 0x0180 */
    uint32_t    res3[31];       /* 0x0184 */
    uint32_t    ispendr0;       /* 0x0200 */
    uint32_t    res4[31];       /* 0x0204 */
    uint32_t    icpendr0;       /* 0x0280 */
    uint32_t    res5[31];       /* 0x0284 */
    uint32_t    isactiver0;     /* 0x0300 */
    uint32_t    res6[31];       /* 0x0304 */
    uint32_t    icactiver0;     /* 0x0380 */
    uint32_t    res7[31];       /* 0x0384 */
    uint32_t    ipriorityrn[8]; /* 0x0400 */
    uint32_t    res8[504];      /* 0x0420 */
    uint32_t    icfgr0;         /* 0x0C00 */
    uint32_t    icfgr1;         /* 0x0C04 */
    uint32_t    res9[62];       /* 0x0C08 */
    uint32_t    igrpmodr0;      /* 0x0D00*/
    uint32_t    res10[63];      /* 0x0D04 */
    uint32_t    nsacr;          /* 0x0E00 */
};

extern volatile struct gic_dist_map *gic_dist;
extern volatile struct gic_rdist_map *gic_rdist_map[CONFIG_MAX_NUM_NODES];
extern volatile struct gic_rdist_sgi_ppi_map *gic_rdist_sgi_ppi_map[CONFIG_MAX_NUM_NODES];

/* Helpers */
static inline void gic_enable_clr(word_t irq)
{
    int word = IRQ_REG(irq);
    int bit = IRQ_BIT(irq);
    /* Using |= here is detrimental to your health */
    if (irq < SPI_START) {
        gic_rdist_sgi_ppi_map[CURRENT_CPU_INDEX()]->icenabler0 = BIT(bit);
    } else {
        gic_dist->icenablern[word] = BIT(bit);
    }

}

static inline void gic_enable_set(word_t irq)
{
    int word = IRQ_REG(irq);
    int bit = IRQ_BIT(irq);

    if (irq < SPI_START) {
        gic_rdist_sgi_ppi_map[CURRENT_CPU_INDEX()]->isenabler0 = BIT(bit);
    } else {
        gic_dist->isenablern[word] = BIT(bit);
    }

}

static inline irq_t getActiveIRQ(void)
{
    irq_t irq;

    if (!IS_IRQ_VALID(active_irq[CURRENT_CPU_INDEX()])) {
        uint32_t val = 0;
        SYSTEM_READ_WORD(ICC_IAR1_EL1, val);
        active_irq[CURRENT_CPU_INDEX()] = val;
    }

    if (IS_IRQ_VALID(active_irq[CURRENT_CPU_INDEX()])) {
        irq = CORE_IRQ_TO_IRQT(CURRENT_CPU_INDEX(), active_irq[CURRENT_CPU_INDEX()] & IRQ_MASK);
    } else {
        irq = irqInvalid;
    }

    return irq;
}

/*
 * GIC has 4 states:
 * seL4 expects two states: active->inactive.
 * We ignore the active state in GIC to conform
 */
/** MODIFIES: phantom_machine_state */
/** DONT_TRANSLATE */
static inline bool_t isIRQPending(void)
{
    uint32_t val = 0;
    /* Check for pending IRQs in group 1: ICC_HPPIR1_EL1 */
    SYSTEM_READ_WORD(ICC_HPPIR1_EL1, val);
    return IS_IRQ_VALID(val);
}

static inline void maskInterrupt(bool_t disable, irq_t irq)
{
#if defined ENABLE_SMP_SUPPORT
    assert(!(IRQ_IS_PPI(irq)) || (IRQT_TO_CORE(irq) == getCurrentCPUIndex()));
#endif

    if (disable) {
        gic_enable_clr(IRQT_TO_IRQ(irq));
    } else {
        gic_enable_set(IRQT_TO_IRQ(irq));
    }
}

static inline void ackInterrupt(irq_t irq)
{
    assert(IS_IRQ_VALID(active_irq[CURRENT_CPU_INDEX()])
           && (active_irq[CURRENT_CPU_INDEX()] & IRQ_MASK) == IRQT_TO_IRQ(irq));

    /* Set End of Interrupt for active IRQ: ICC_EOIR1_EL1 */
    SYSTEM_WRITE_WORD(ICC_EOIR1_EL1, active_irq[CURRENT_CPU_INDEX()]);
    active_irq[CURRENT_CPU_INDEX()] = IRQ_NONE;

}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

extern word_t gic_vcpu_num_list_regs;

static inline uint32_t get_gic_vcpu_ctrl_hcr(void)
{
    uint32_t reg;
    MRS(ICH_HCR_EL2, reg);
    return reg;
}

static inline void set_gic_vcpu_ctrl_hcr(uint32_t hcr)
{
    MSR(ICH_HCR_EL2, hcr);
}

static inline uint32_t get_gic_vcpu_ctrl_vmcr(void)
{
    uint32_t reg;
    MRS(ICH_VMCR_EL2, reg);
    return reg;
}

static inline void set_gic_vcpu_ctrl_vmcr(uint32_t vmcr)
{
    MSR(ICH_VMCR_EL2, vmcr);
}

/* Note: On the GICv3 there are potentially up to 128 preemption
 * levels, and as such up to 4 APR registers.
 *
 * At this point the code assumes a maximum of 32 preemption levels.
 */
static inline uint32_t get_gic_vcpu_ctrl_apr(void)
{
    uint32_t reg;
    MRS(ICH_AP1R0_EL2, reg);
    return reg;
}

static inline void set_gic_vcpu_ctrl_apr(uint32_t apr)
{
    MSR(ICH_AP1R0_EL2, apr);
}

static inline uint32_t get_gic_vcpu_ctrl_vtr(void)
{
    uint32_t reg;
    MRS(ICH_VTR_EL2, reg);
    return reg;
}

static inline uint32_t get_gic_vcpu_ctrl_eisr0(void)
{
    uint32_t reg;
    MRS(ICH_EISR_EL2, reg);
    return reg;
}

/* Note: GICv3 supports a maximum of 16 list registers
 * so there is no need to support EISR1 on GICv3.
 */
static inline uint32_t get_gic_vcpu_ctrl_eisr1(void)
{
    return 0;
}

static inline uint32_t get_gic_vcpu_ctrl_misr(void)
{
    uint32_t reg;
    MRS(ICH_MISR_EL2, reg);
    return reg;
}

static inline virq_t get_gic_vcpu_ctrl_lr(int num)
{
    virq_t virq;
    uint64_t reg = 0;
    switch (num) {
    case 0:
        MRS(ICH_LR0_EL2, reg);
        break;
    case 1:
        MRS(ICH_LR1_EL2, reg);
        break;
    case 2:
        MRS(ICH_LR2_EL2, reg);
        break;
    case 3:
        MRS(ICH_LR3_EL2, reg);
        break;
    case 4:
        MRS(ICH_LR4_EL2, reg);
        break;
    case 5:
        MRS(ICH_LR5_EL2, reg);
        break;
    case 6:
        MRS(ICH_LR6_EL2, reg);
        break;
    case 7:
        MRS(ICH_LR7_EL2, reg);
        break;
    case 8:
        MRS(ICH_LR8_EL2, reg);
        break;
    case 9:
        MRS(ICH_LR9_EL2, reg);
        break;
    case 10:
        MRS(ICH_LR10_EL2, reg);
        break;
    case 11:
        MRS(ICH_LR11_EL2, reg);
        break;
    case 12:
        MRS(ICH_LR12_EL2, reg);
        break;
    case 13:
        MRS(ICH_LR13_EL2, reg);
        break;
    case 14:
        MRS(ICH_LR14_EL2, reg);
        break;
    case 15:
        MRS(ICH_LR15_EL2, reg);
        break;
    default:
        fail("gicv3: invalid lr");
    }
    virq.words[0] = reg;
    return virq;
}

static inline void set_gic_vcpu_ctrl_lr(int num, virq_t lr)
{
    uint64_t reg = lr.words[0];
    switch (num) {
    case 0:
        MSR(ICH_LR0_EL2, reg);
        break;
    case 1:
        MSR(ICH_LR1_EL2, reg);
        break;
    case 2:
        MSR(ICH_LR2_EL2, reg);
        break;
    case 3:
        MSR(ICH_LR3_EL2, reg);
        break;
    case 4:
        MSR(ICH_LR4_EL2, reg);
        break;
    case 5:
        MSR(ICH_LR5_EL2, reg);
        break;
    case 6:
        MSR(ICH_LR6_EL2, reg);
        break;
    case 7:
        MSR(ICH_LR7_EL2, reg);
        break;
    case 8:
        MSR(ICH_LR8_EL2, reg);
        break;
    case 9:
        MSR(ICH_LR9_EL2, reg);
        break;
    case 10:
        MSR(ICH_LR10_EL2, reg);
        break;
    case 11:
        MSR(ICH_LR11_EL2, reg);
        break;
    case 12:
        MSR(ICH_LR12_EL2, reg);
        break;
    case 13:
        MSR(ICH_LR13_EL2, reg);
        break;
    case 14:
        MSR(ICH_LR14_EL2, reg);
        break;
    case 15:
        MSR(ICH_LR15_EL2, reg);
        break;
    default:
        fail("gicv3: invalid lr");
    }
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */
