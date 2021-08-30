/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/*
 * ARM Generic Interrupt Controller PL-390
 */
#pragma once

/* tell the kernel we have the set trigger feature */
#define HAVE_SET_TRIGGER 1

#include <stdint.h>
#include <util.h>
#include <linker.h>
#include <mode/smp/smp.h>
#include <model/statedata.h>

#include "gic_common.h"

#define IRQ_MASK MASK(10u)
#define GIC_VCPU_MAX_NUM_LR 64

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
    uint32_t enable;                /* 0x000 */
    uint32_t ic_type;               /* 0x004 */
    uint32_t dist_ident;            /* 0x008 */
    uint32_t res1[29];              /* [0x00C, 0x080) */

    uint32_t security[32];          /* [0x080, 0x100) */

    uint32_t enable_set[32];        /* [0x100, 0x180) */
    uint32_t enable_clr[32];        /* [0x180, 0x200) */
    uint32_t pending_set[32];       /* [0x200, 0x280) */
    uint32_t pending_clr[32];       /* [0x280, 0x300) */
    uint32_t active[32];            /* [0x300, 0x380) */
    uint32_t res2[32];              /* [0x380, 0x400) */

    uint32_t priority[255];         /* [0x400, 0x7FC) */
    uint32_t res3;                  /* 0x7FC */

    uint32_t targets[255];            /* [0x800, 0xBFC) */
    uint32_t res4;                  /* 0xBFC */

    uint32_t config[64];             /* [0xC00, 0xD00) */

    uint32_t spi[32];               /* [0xD00, 0xD80) */
    uint32_t res5[20];              /* [0xD80, 0xDD0) */
    uint32_t res6;                  /* 0xDD0 */
    uint32_t legacy_int;            /* 0xDD4 */
    uint32_t res7[2];               /* [0xDD8, 0xDE0) */
    uint32_t match_d;               /* 0xDE0 */
    uint32_t enable_d;              /* 0xDE4 */
    uint32_t res8[70];               /* [0xDE8, 0xF00) */

    uint32_t sgi_control;           /* 0xF00 */
    uint32_t res9[3];               /* [0xF04, 0xF10) */
    uint32_t sgi_pending_clr[4];    /* [0xF10, 0xF20) */
    uint32_t res10[40];             /* [0xF20, 0xFC0) */

    uint32_t periph_id[12];         /* [0xFC0, 0xFF0) */
    uint32_t component_id[4];       /* [0xFF0, 0xFFF] */
};

/* Memory map for GIC  cpu interface */
struct gic_cpu_iface_map {
    uint32_t icontrol;              /*  0x000         */
    uint32_t pri_msk_c;             /*  0x004         */
    uint32_t pb_c;                  /*  0x008         */
    uint32_t int_ack;               /*  0x00C         */
    uint32_t eoi;                   /*  0x010         */
    uint32_t run_priority;          /*  0x014         */
    uint32_t hi_pend;               /*  0x018         */
    uint32_t ns_alias_bp_c;         /*  0x01C         */
    uint32_t ns_alias_ack;          /*  0x020 GIC400 only */
    uint32_t ns_alias_eoi;          /*  0x024 GIC400 only */
    uint32_t ns_alias_hi_pend;      /*  0x028 GIC400 only */

    uint32_t res1[5];               /* [0x02C, 0x040) */

    uint32_t integ_en_c;            /*  0x040 PL390 only */
    uint32_t interrupt_out;         /*  0x044 PL390 only */
    uint32_t res2[2];               /* [0x048, 0x050)    */

    uint32_t match_c;               /*  0x050 PL390 only */
    uint32_t enable_c;              /*  0x054 PL390 only */

    uint32_t res3[30];              /* [0x058, 0x0FC)  */
    uint32_t active_priority[4];    /* [0x0D0, 0xDC] GIC400 only */
    uint32_t ns_active_priority[4]; /* [0xE0,0xEC] GIC400 only */
    uint32_t res4[3];

    uint32_t cpu_if_ident;          /*  0x0FC         */
    uint32_t res5[948];             /* [0x100. 0xFC0) */

    uint32_t periph_id[8];          /* [0xFC0, 9xFF0) PL390 only */
    uint32_t component_id[4];       /* [0xFF0, 0xFFF] PL390 only */
};

extern volatile struct gic_dist_map *const gic_dist;
extern volatile struct gic_cpu_iface_map *const gic_cpuiface;

/* Helpers */
static inline int is_irq_edge_triggered(word_t irq)
{
    int word = irq >> 4;
    int bit = ((irq & 0xf) * 2);
    return !!(gic_dist->config[word] & BIT(bit + 1));
}

static inline void dist_pending_clr(word_t irq)
{
    int word = IRQ_REG(irq);
    int bit = IRQ_BIT(irq);
    /* Using |= here is detrimental to your health */
    gic_dist->pending_clr[word] = BIT(bit);
}

static inline void dist_enable_clr(word_t irq)
{
    int word = IRQ_REG(irq);
    int bit = IRQ_BIT(irq);
    /* Using |= here is detrimental to your health */
    gic_dist->enable_clr[word] = BIT(bit);
}

static inline void dist_enable_set(word_t irq)
{
    int word = IRQ_REG(irq);
    int bit = IRQ_BIT(irq);
    gic_dist->enable_set[word] = BIT(bit);
}

static inline irq_t getActiveIRQ(void)
{
    irq_t irq;
    if (!IS_IRQ_VALID(active_irq[CURRENT_CPU_INDEX()])) {
        active_irq[CURRENT_CPU_INDEX()] = gic_cpuiface->int_ack;
    }

    if (IS_IRQ_VALID(active_irq[CURRENT_CPU_INDEX()])) {
        irq = CORE_IRQ_TO_IRQT(CURRENT_CPU_INDEX(), active_irq[CURRENT_CPU_INDEX()] & IRQ_MASK);
    } else {
        irq = irqInvalid;
    }

    return irq;
}

/*
 * GIC has 4 states: pending->active(+pending)->inactive
 * seL4 expects two states: active->inactive.
 * We ignore the active state in GIC to conform
 */
static inline bool_t isIRQPending(void)
{
    return IS_IRQ_VALID(gic_cpuiface->hi_pend);
}

static inline void maskInterrupt(bool_t disable, irq_t irq)
{
#if defined ENABLE_SMP_SUPPORT && defined CONFIG_ARCH_ARM
    assert(!(IRQ_IS_PPI(irq)) || (IRQT_TO_CORE(irq) == getCurrentCPUIndex()));
#endif
    if (disable) {
        dist_enable_clr(IRQT_TO_IRQ(irq));
    } else {
        dist_enable_set(IRQT_TO_IRQ(irq));
    }
}

static inline void ackInterrupt(irq_t irq)
{
    assert(IS_IRQ_VALID(active_irq[CURRENT_CPU_INDEX()])
           && (active_irq[CURRENT_CPU_INDEX()] & IRQ_MASK) == IRQT_TO_IRQ(irq));
    if (is_irq_edge_triggered(IRQT_TO_IRQ(irq))) {
        dist_pending_clr(IRQT_TO_IRQ(irq));
    }
    gic_cpuiface->eoi = active_irq[CURRENT_CPU_INDEX()];
    active_irq[CURRENT_CPU_INDEX()] = IRQ_NONE;

}


#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

/* GIC VCPU Control Interface */
struct gich_vcpu_ctrl_map {
    uint32_t hcr;    /* 0x000 RW 0x00000000 Hypervisor Control Register */
    uint32_t vtr;    /* 0x004 RO IMPLEMENTATION DEFINED VGIC Type Register */
    /* Save restore on VCPU switch */
    uint32_t vmcr;   /* 0x008 RW IMPLEMENTATION DEFINED Virtual Machine Control Register */
    uint32_t res1[1];
    /* IRQ pending flags */
    uint32_t misr;   /* 0x010 RO 0x00000000 Maintenance Interrupt Status Register */
    uint32_t res2[3];
    /* Bitfield of list registers that have EOI */
    uint32_t eisr0;  /* 0x020 RO 0x00000000 End of Interrupt Status Registers 0 and 1, see EISRn */
    uint32_t eisr1;  /* 0x024 RO 0x00000000 */
    uint32_t res3[2];
    /* Bitfield of list registers that are empty */
    uint32_t elsr0;  /* 0x030 RO IMPLEMENTATION DEFINED a */
    uint32_t elsr1;  /* 0x034 RO IMPLEMENTATION DEFINED a Empty List Register Status Registers 0 and 1, see ELRSRn */
    uint32_t res4[46];
    /* Active priority: bitfield of active priorities */
    uint32_t apr;    /* 0x0F0 RW 0x00000000 Active Priorities Register */
    uint32_t res5[3];
    uint32_t lr[64]; /* 0x100 RW 0x00000000 List Registers 0-63, see LRn */
};

extern volatile struct gich_vcpu_ctrl_map *gic_vcpu_ctrl;
extern unsigned int gic_vcpu_num_list_regs;

static inline uint32_t get_gic_vcpu_ctrl_hcr(void)
{
    return gic_vcpu_ctrl->hcr;
}

static inline void set_gic_vcpu_ctrl_hcr(uint32_t hcr)
{
    gic_vcpu_ctrl->hcr = hcr;
}

static inline uint32_t get_gic_vcpu_ctrl_vmcr(void)
{
    return gic_vcpu_ctrl->vmcr;
}

static inline void set_gic_vcpu_ctrl_vmcr(uint32_t vmcr)
{
    gic_vcpu_ctrl->vmcr = vmcr;
}

static inline uint32_t get_gic_vcpu_ctrl_apr(void)
{
    return gic_vcpu_ctrl->apr;
}

static inline void set_gic_vcpu_ctrl_apr(uint32_t apr)
{
    gic_vcpu_ctrl->apr = apr;
}

static inline uint32_t get_gic_vcpu_ctrl_vtr(void)
{
    return gic_vcpu_ctrl->vtr;
}

static inline uint32_t get_gic_vcpu_ctrl_eisr0(void)
{
    return gic_vcpu_ctrl->eisr0;
}

static inline uint32_t get_gic_vcpu_ctrl_eisr1(void)
{
    return gic_vcpu_ctrl->eisr1;
}

static inline uint32_t get_gic_vcpu_ctrl_misr(void)
{
    return gic_vcpu_ctrl->misr;
}

static inline virq_t get_gic_vcpu_ctrl_lr(int num)
{
    virq_t virq;
    virq.words[0] = gic_vcpu_ctrl->lr[num];
    return virq;
}

static inline void set_gic_vcpu_ctrl_lr(int num, virq_t lr)
{
    gic_vcpu_ctrl->lr[num] = lr.words[0];
}

#endif /* End of CONFIG_ARM_HYPERVISOR_SUPPORT */
