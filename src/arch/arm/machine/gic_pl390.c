/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/machine/gic_pl390.h>


/* Setters/getters helpers */
#define IRQ_REG(IRQ) ((IRQ) / 32)
#define IRQ_BIT(IRQ) BIT((IRQ) % 32)
#define IRQ_MASK MASK(10)
#define IS_IRQ_VALID(X) (((X)&IRQ_MASK) < SPECIAL_IRQ_START)

#define CPU(X) (1<<(X))
#define TARGET_CPU_ALLINT(CPU) ( \
        ( ((CPU)&0xff)<<0  ) |\
        ( ((CPU)&0xff)<<8  ) |\
        ( ((CPU)&0xff)<<16 ) |\
        ( ((CPU)&0xff)<<24 ) \
    )
#define TARGET_CPU0_ALLINT   TARGET_CPU_ALLINT(CPU(0))


#define IRQ_SET_ALL 0xffffffff;

/* Special IRQ's */
#define SPECIAL_IRQ_START 1020
#define IRQ_NONE          1023

/* Memory map for GIC distributer */
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
    uint32_t alias_nsbp_c;          /*  0x01C         */

    uint32_t res1[8];               /* [0x020, 0x040) */

    uint32_t integ_en_c;            /*  0x040         */
    uint32_t interrupt_out;         /*  0x044         */
    uint32_t res2[2];               /* [0x048, 0x050) */

    uint32_t match_c;               /*  0x050         */
    uint32_t enable_c;              /*  0x054         */

    uint32_t res3[41];              /* [0x58, 0x0FC)  */

    uint32_t cpu_if_ident;          /*  0x0FC         */
    uint32_t res4[943];             /* [0x100. 0xFBC) */

    uint32_t periph_id[8];          /* [0xFC0, 9xFF0) */
    uint32_t component_id[4];       /* [0xFF0, 0xFFF] */
};

#ifndef GIC_PL390_DISTRIBUTOR_PPTR
#error GIC_PL390_DISTRIBUTOR_PPTR must be defined for virtual memory access to the gic distributer
#else  /* GIC_DISTRIBUTOR_PPTR */
volatile struct gic_dist_map *gic_dist =
    (volatile struct gic_dist_map*)(GIC_PL390_DISTRIBUTOR_PPTR);
#endif /* GIC_DISTRIBUTOR_PPTR */

#ifndef GIC_PL390_CONTROLLER_PPTR
#error GIC_PL390_CONTROLLER_PPTR must be defined for virtual memory access to the gic cpu interface
#else  /* GIC_CONTROLLER_PPTR */
volatile struct gic_cpu_iface_map *gic_cpuiface =
    (volatile struct gic_cpu_iface_map*)(GIC_PL390_CONTROLLER_PPTR);
#endif /* GIC_CONTROLLER_PPTR */


/* Helpers */
static inline int
is_irq_pending(irq_t irq)
{
    int word = irq / 32;
    int bit = irq & 0x1f;
    return !!(gic_dist->pending_set[word] & BIT(bit));
}

static inline int
is_irq_active(irq_t irq)
{
    int word = irq / 32;
    int bit = irq & 0x1f;
    return !!(gic_dist->active[word] & BIT(bit));
}

static inline int
is_irq_enabled(irq_t irq)
{
    int word = irq / 32;
    int bit = irq & 0x1f;
    return !!(gic_dist->enable_set[word] & BIT(bit));
}

static inline int
is_irq_edge_triggered(irq_t irq)
{
    int word = irq / 16;
    int bit = ((irq & 0xf) * 2);
    return !!(gic_dist->config[word] & BIT(bit + 1));
}

static inline int
is_irq_1_N(irq_t irq)
{
    int word = irq / 16;
    int bit = ((irq & 0xf) * 2);
    return !!(gic_dist->config[word] & BIT(bit + 0));
}

static inline int
is_irq_N_N(irq_t irq)
{
    return !(is_irq_1_N(irq));
}

static inline void
dist_pending_clr(irq_t irq)
{
    int word = irq / 32;
    int bit = irq & 0x1f;
    /* Using |= here is detrimental to your health */
    gic_dist->pending_clr[word] = BIT(bit);
}

static inline void
dist_pending_set(irq_t irq)
{
    int word = irq / 32;
    int bit = irq & 0x1f;
    gic_dist->pending_set[word] = BIT(bit);
}

static inline void
dist_enable_clr(irq_t irq)
{
    int word = irq / 32;
    int bit = irq & 0x1f;
    /* Using |= here is detrimental to your health */
    gic_dist->enable_clr[word] = BIT(bit);
}

static inline void
dist_enable_set(irq_t irq)
{
    int word = irq / 32;
    int bit = irq & 0x1f;
    gic_dist->enable_set[word] = BIT(bit);
}



/**
   DONT_TRANSLATE
 */
BOOT_CODE static void
dist_init(void)
{
    int i;
    int nirqs = 32 * ((gic_dist->ic_type & 0x1f) + 1);
    gic_dist->enable = 0;

    for (i = 0; i < nirqs; i += 32) {
        /* disable */
        gic_dist->enable_clr[i / 32] = IRQ_SET_ALL;
        /* clear pending */
        gic_dist->pending_clr[i / 32] = IRQ_SET_ALL;
    }

    /* reset interrupts priority */
    for (i = 32; i < nirqs; i += 4) {
        gic_dist->priority[i / 4] = 0x0;
    }

    /* reset int target to cpu 0 only */
    for (i = 0; i < nirqs; i += 4) {
        gic_dist->targets[i / 4] = TARGET_CPU0_ALLINT;
    }

    /* level-triggered, 1-N */
    for (i = 64; i < nirqs; i += 32) {
        gic_dist->config[i / 32] = 0x55555555;
    }
    /* configure to group 0 for security */
    for (i = 0; i < nirqs; i += 32) {
        gic_dist->security[i / 32] = 0;
    }
    /* enable the int controller */
    gic_dist->enable = 1;
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE static void
cpu_iface_init(void)
{
    uint32_t i;

    /* the registers are banked per CPU, need to clear them */
    gic_dist->enable_clr[0] = IRQ_SET_ALL;
    gic_dist->pending_clr[0] = IRQ_SET_ALL;
    gic_dist->priority[0] = 0x0;
    /* put everything in group 0 */

    /* clear any software generated interrupts */
    for (i = 0; i < 16; i += 4) {
        gic_dist->sgi_pending_clr[i / 4] = IRQ_SET_ALL;
    }

    gic_cpuiface->icontrol = 0;
    gic_cpuiface->pri_msk_c = 0x000000f0;
    gic_cpuiface->pb_c = 0x00000003;

    while ( (i = gic_cpuiface->int_ack) != IRQ_NONE) {
        gic_cpuiface->eoi = i;
    }

    gic_cpuiface->icontrol = 1;
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initIRQController(void)
{
    dist_init();
    cpu_iface_init();
}



/*
 * The only sane way to get an GIC IRQ number that can be properly ACKED later is
 * through the int_ack register. Unfortunately, reading this register changes the
 * interrupt state to pending so future reads will not return the same value
 * For this reason, we have a global variable to store the IRQ number.
 */
static uint32_t active_irq = IRQ_NONE;

/**
   DONT_TRANSLATE
 */
interrupt_t
getActiveIRQ(void)
{
    uint32_t irq;
    if (!IS_IRQ_VALID(active_irq)) {
        active_irq = gic_cpuiface->int_ack;
    }

    if (IS_IRQ_VALID(active_irq)) {
        irq = active_irq & IRQ_MASK;
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
/**
   DONT_TRANSLATE
 */
bool_t
isIRQPending(void)
{
    return IS_IRQ_VALID(gic_cpuiface->hi_pend);
}


/**
   DONT_TRANSLATE
 */
void
maskInterrupt(bool_t disable, interrupt_t irq)
{
    if (disable) {
        dist_enable_clr(irq);
    } else {
        dist_enable_set(irq);
    }
}

/**
   DONT_TRANSLATE
 */
void
ackInterrupt(irq_t irq)
{
    assert(IS_IRQ_VALID(active_irq) && (active_irq & IRQ_MASK) == irq);
    if (is_irq_edge_triggered(irq)) {
        dist_pending_clr(irq);
    }
    gic_cpuiface->eoi = active_irq;
    active_irq = IRQ_NONE;
}

void
handleSpuriousIRQ(void)
{
}
