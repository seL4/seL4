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
#endif

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
    uint32_t res9[5235];            /* [0x0F30, 0x6100) */

    uint64_t iroutern[960];         /* [0x6100, 0x7F00) */
};

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

extern volatile struct gic_dist_map *const gic_dist;
extern volatile struct gic_rdist_map *gic_rdist_map[CONFIG_MAX_NUM_NODES];
extern volatile struct gic_rdist_sgi_ppi_map *gic_rdist_sgi_ppi_map[CONFIG_MAX_NUM_NODES];

/* Helpers */
static inline int is_irq_edge_triggered(word_t irq)
{
    uint32_t icfgr = 0;
    int word = irq >> 4;
    int bit = ((irq & 0xf) * 2);

    if (HW_IRQ_IS_SGI(irq)) {
        return 0;
    }
    if (HW_IRQ_IS_PPI(irq)) {
        icfgr = gic_rdist_sgi_ppi_map[CURRENT_CPU_INDEX()]->icfgr1;
    } else {
        icfgr = gic_dist->icfgrn[word];
    }

    return !!(icfgr & BIT(bit + 1));
}

static inline void gic_pending_clr(word_t irq)
{
    int word = IRQ_REG(irq);
    int bit = IRQ_BIT(irq);
    /* Using |= here is detrimental to your health */
    /* Applicable for SPI and PPIs */
    if (irq < SPI_START) {
        gic_rdist_sgi_ppi_map[CURRENT_CPU_INDEX()]->icpendr0 = BIT(bit);
    } else {
        gic_dist->icpendrn[word] = BIT(bit);
    }
}

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
    word_t hw_irq = IRQT_TO_IRQ(irq);
    assert(IS_IRQ_VALID(active_irq[CURRENT_CPU_INDEX()]) && (active_irq[CURRENT_CPU_INDEX()] & IRQ_MASK) == hw_irq);

    if (is_irq_edge_triggered(hw_irq)) {
        gic_pending_clr(hw_irq);
    }

    /* Set End of Interrupt for active IRQ: ICC_EOIR1_EL1 */
    SYSTEM_WRITE_WORD(ICC_EOIR1_EL1, active_irq[CURRENT_CPU_INDEX()]);
    active_irq[CURRENT_CPU_INDEX()] = IRQ_NONE;

}

