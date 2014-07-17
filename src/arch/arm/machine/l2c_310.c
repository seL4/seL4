/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <arch/machine/l2c_310.h>

#define L2_LINE_SIZE_BITS 5
#define L2_LINE_SIZE BIT(L2_LINE_SIZE_BITS) /* 32 byte line size */

#define L2_LINE_START(a) ROUND_DOWN(a, L2_LINE_SIZE_BITS)

compile_assert(l2_l1_same_line_size, L2_LINE_SIZE_BITS == L1_CACHE_LINE_SIZE_BITS)

/* MSHIELD Control */
#define MSHIELD_SMC_ROM_CTRL_CTRL         0x102
#define MSHIELD_SMC_ROM_CTRL_AUX          0x109
#define MSHIELD_SMC_ROM_CTRL_LATENCY      0x112
/* MSHIELD Address Filter */
#define MSHIELD_SMC_ROM_ADDR_FILT_START   /* ? */
#define MSHIELD_SMC_ROM_ADDR_FILT_END     /* ? */
/* MSHIELD Control 2 */
#define MSHIELD_SMC_ROM_CTRL2_DEBUG       0x100
#define MSHIELD_SMC_ROM_CTRL2_PREFETCH    0x113 /* ? */
#define MSHIELD_SMC_ROM_CTRL2_POWER       /* ? */
/* MSHIELD Cache maintenance */
#define MSHIELD_SMC_ROM_MAINT_INVALIDATE  0x101


/* Cache ID */
#define PL310_LOCKDOWN_BY_MASK            (0xf<<25)
#define PL310_LOCKDOWN_BY_MASTER          (0xe<<25)
#define PL310_LOCKDOWN_BY_LINE            (0xf<<25)

/* Primary control */
#define CTRL_CTRL_EN BIT(0)

/* Auxiliary control */
#define CTRL_AUX_EARLY_BRESP_EN                            BIT(30)
#define CTRL_AUX_IPREFETCH_EN                              BIT(29)
#define CTRL_AUX_DPREFETCH_EN                              BIT(28)
#define CTRL_AUX_NSECURE_INT_ACCESS                        BIT(27)
#define CTRL_AUX_NSECURE_LOCKDOWN_EN                       BIT(26)
#define CTRL_AUX_REPLACEMENT_POLICY                        BIT(25)
#define CTRL_AUX_FORCE_WR_ALLOC(X)            (((X)&0x3) * BIT(23)
#define CTRL_AUX_SHARED_ATTRIB_OVERRIDE_EN                 BIT(22)
#define CTRL_AUX_PARITY_EN                                 BIT(21)
#define CTRL_AUX_EVENT_MONITOR_BUS_EN                      BIT(20)
#define CTRL_AUX_WAYSIZE(X)                   (((X)&0x7) * BIT(17) )
#define CTRL_AUX_ASSOCIATIVITY                             BIT(16)
#define CTRL_AUX_SHARED_ATTRIB_INVALIDATE_EN               BIT(13)
#define CTRL_AUX_EXCLUSIVE_CACHE_CONFIG                    BIT(12)
#define CTRL_AUX_STOREBUFDEV_LIMIT_EN                      BIT(11)
#define CTRL_AUX_HIGH_PRIO_SODEV_EN                        BIT(10)
#define CTRL_AUX_FULL_LINE_ZEROS_ENABLE                    BIT( 0)

#define CTRL_AUX_WAYSIZE_16K         CTRL_AUX_WAYSIZE(1)
#define CTRL_AUX_WAYSIZE_32K         CTRL_AUX_WAYSIZE(2)
#define CTRL_AUX_WAYSIZE_64K         CTRL_AUX_WAYSIZE(3)
#define CTRL_AUX_WAYSIZE_128K        CTRL_AUX_WAYSIZE(4)
#define CTRL_AUX_WAYSIZE_256K        CTRL_AUX_WAYSIZE(5)
#define CTRL_AUX_WAYSIZE_512K        CTRL_AUX_WAYSIZE(6)

#define CTRL_AUX_ASSOCIATIVITY_8WAY   (0 * CTRL_AUX_ASSOCIATIVITY)
#define CTRL_AUX_ASSOCIATIVITY_16WAY  (1 * CTRL_AUX_ASSOCIATIVITY)

#define CTRL_AUS_REPLPOLICY_RROBIN    (0 * CTRL_AUX_REPLACEMENT_POLICY)
#define CTRL_AUS_REPLPOLICY_PRAND     (1 * CTRL_AUX_REPLACEMENT_POLICY)

/* Latency */
#define CTRL_RAM_LATENCY_SET(X,S)    (((X)&0x7) * BIT(S))
#define CTRL_RAM_LATENCY_SETUP(X)    CTRL_RAM_LATENCY_SET(X, 0)
#define CTRL_RAM_LATENCY_READ(X)     CTRL_RAM_LATENCY_SET(X, 4)
#define CTRL_RAM_LATENCY_WRITE(X)    CTRL_RAM_LATENCY_SET(X, 8)

#define CTRL_RAM_LATENCY(W,R,S)    ( CTRL_RAM_LATENCY_SETUP(S) \
                                   | CTRL_RAM_LATENCY_READ(R)  \
                                   | CTRL_RAM_LATENCY_WRITE(W) )


/* Maintenance */
#define MAINTENANCE_PENDING          BIT(0)

/* POWER */
#define CTRL2_PWR_DYNAMIC_CLK_EN     BIT(1)
#define CTRL2_PWR_STANDBY_ON         BIT(0)

/* PREFECTCH */
#define CTRL2_PFET_DBL_LINEFILL_EN              BIT(30)
#define CTRL2_PFET_INST_PREFETCH_EN             BIT(29)
#define CTRL2_PFET_DATA_PREFETCH_EN             BIT(28)
#define CTRL2_PFET_DBL_LINEFILL_ON_WRAP_EN      BIT(27)
#define CTRL2_PFET_PREFETCH_DROP_EN             BIT(24)
#define CTRL2_PFET_INCR_DBL_LINEFILL_EN         BIT(23)
#define CTRL2_PFET_NOT_SAME_ID_ON_EXCL_SEQ_EN   BIT(21)
#define CTRL2_PFET_PREFETCH_OFFSET(X)    ((X) * BIT( 0) )


struct l2cc_map {

    struct {
        uint32_t cache_id;              /* 0x000 */
        uint32_t cache_type;            /* 0x004 */
        uint32_t res[62];
    } id /* reg0 */;

    struct {
        uint32_t control;               /* 0x100 */
        uint32_t aux_control;           /* 0x104 */
        uint32_t tag_ram_control;       /* 0x108 */
        uint32_t data_ram_control;      /* 0x10C */
        uint32_t res[60];
    } control /* reg1 */;

    struct {
        uint32_t ev_counter_ctrl;       /* 0x200 */
        uint32_t ev_counter1_cfg;       /* 0x204 */
        uint32_t ev_counter0_cfg;       /* 0x208 */
        uint32_t ev_counter1;           /* 0x20C */
        uint32_t ev_counter0;           /* 0x210 */
        uint32_t int_mask;              /* 0x214 */
        uint32_t int_mask_status;       /* 0x218 */
        uint32_t int_raw_status;        /* 0x21C */
        uint32_t int_clear;             /* 0x220 */
        uint32_t res[55];
    } interrupt /* reg2 */;

    struct {
        uint32_t res[64];
    } reg3;
    struct {
        uint32_t res[64];
    } reg4;
    struct {
        uint32_t res[64];
    } reg5;
    struct {
        uint32_t res[64];
    } reg6;

    struct {
        uint32_t res[12];
        uint32_t cache_sync;            /* 0x730 */
        uint32_t res1[15];
        uint32_t inv_pa;                /* 0x770 */
        uint32_t res2[2];
        uint32_t inv_way;               /* 0x77C */
        uint32_t res3[12];
        uint32_t clean_pa;              /* 0x7B0 */
        uint32_t res4[1];
        uint32_t clean_index;           /* 0x7B8 */
        uint32_t clean_way;             /* 0x7BC */
        uint32_t res5[12];
        uint32_t clean_inv_pa;          /* 0x7F0 */
        uint32_t res6[1];
        uint32_t clean_inv_index;       /* 0x7F8 */
        uint32_t clean_inv_way;         /* 0x7FC */
    } maintenance /* reg7 */;

    struct {
        uint32_t res[64];
    } reg8;

    struct {
        uint32_t d_lockdown0;           /* 0x900 */
        uint32_t i_lockdown0;           /* 0x904 */
        uint32_t d_lockdown1;           /* 0x908 */
        uint32_t i_lockdown1;           /* 0x90C */
        uint32_t d_lockdown2;           /* 0x910 */
        uint32_t i_lockdown2;           /* 0x914 */
        uint32_t d_lockdown3;           /* 0x918 */
        uint32_t i_lockdown3;           /* 0x91C */
        uint32_t d_lockdown4;           /* 0x920 */
        uint32_t i_lockdown4;           /* 0x924 */
        uint32_t d_lockdown5;           /* 0x928 */
        uint32_t i_lockdown5;           /* 0x92C */
        uint32_t d_lockdown6;           /* 0x930 */
        uint32_t i_lockdown6;           /* 0x934 */
        uint32_t d_lockdown7;           /* 0x938 */
        uint32_t i_lockdown7;           /* 0x93C */
        uint32_t res[4];
        uint32_t lock_line_eng;         /* 0x950 */
        uint32_t unlock_wayg;           /* 0x954 */
        uint32_t res1[42];
    } lockdown /* reg9 */;

    struct {
        uint32_t res[64];
    } reg10;
    struct {
        uint32_t res[64];
    } reg11;

    struct {
        uint32_t addr_filtering_start;  /* 0xC00 */
        uint32_t addr_filtering_end;    /* 0xC04 */
        uint32_t res[62];
    } filter /* reg12 */;

    struct {
        uint32_t res[64];
    } reg13;
    struct {
        uint32_t res[64];
    } reg14;

    struct {
        uint32_t res[16];
        uint32_t debug_ctrl;            /* 0xF40 */
        uint32_t res1[7];
        uint32_t prefetch_ctrl;         /* 0xF60 */
        uint32_t res2[7];
        uint32_t power_ctrl;            /* 0xF80 */
        uint32_t res3[31];
    } control2 /* reg15 */;
};


#ifndef L2CC_L2C310_PPTR
#error L2CC_L2C310_PPTR must be defined for virtual memory access to the L2 cache controller
#else  /* L2CC_PPTR */
volatile struct l2cc_map *l2cc = (volatile struct l2cc_map*)L2CC_L2C310_PPTR;
#endif /* !L2CC_PPTR */


#ifdef TI_MSHIELD
/**
   DONT_TRANSLATE
 */
BOOT_CODE static void
mshield_smc(uint32_t callid, uint32_t arg1, uint32_t arg2)
{
    register uint32_t _arg1 asm ("r0") = arg1;
    register uint32_t _arg2 asm ("r1") = arg2;
    register uint32_t _callid asm ("r12") = callid;
    asm volatile ("push {r2-r12, lr}\n"
                  "dsb\n"
                  "smc #0\n"
                  "pop {r2-r12, lr}"
                  :: "r"(_callid), "r"(_arg1), "r"(_arg2));
}
#endif /* TI_MSHIELD */

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initL2Cache(void)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    uint32_t aux;
    uint32_t tag_ram;
    uint32_t data_ram;
    uint32_t prefetch;

    prefetch = CTRL2_PFET_INST_PREFETCH_EN | CTRL2_PFET_DATA_PREFETCH_EN;
#if defined(IMX6)
    tag_ram  = CTRL_RAM_LATENCY(1, 2, 1);
    data_ram = CTRL_RAM_LATENCY(1, 2, 1);
#else
    tag_ram  = CTRL_RAM_LATENCY(1, 1, 0);
    data_ram = CTRL_RAM_LATENCY(1, 2, 0);
#endif

    aux      = 0
               | CTRL_AUX_IPREFETCH_EN
               | CTRL_AUX_DPREFETCH_EN
               | CTRL_AUX_NSECURE_INT_ACCESS
               | CTRL_AUX_NSECURE_LOCKDOWN_EN
               | CTRL_AUX_ASSOCIATIVITY_16WAY
               | CTRL_AUS_REPLPOLICY_RROBIN;

#if defined(EXYNOS4)
    aux |= CTRL_AUX_WAYSIZE_64K;

#elif defined(OMAP4) /* ! EXYNOS4 */
    aux |= CTRL_AUX_WAYSIZE_32K;

#elif defined(IMX6) /* ! (EXYNOS4 || OMAP4) */
    aux |= CTRL_AUX_WAYSIZE_64K;

#else /* ! (EXYNOS4 || OMAP4 || IMX6) */
#error Unknown platform for L2C-310
#endif /* EXYNOS4 || OMAP4 || IMX6 */

#ifdef TI_MSHIELD
    /* Access secure registers through Security Middleware Call */
    /* 1: Write to aux Tag RAM latentcy, Data RAM latency, prefect, power control registers  */
    mshield_smc(MSHIELD_SMC_ROM_CTRL_CTRL, 0, 0);
    mshield_smc(MSHIELD_SMC_ROM_CTRL_AUX , aux, 0);
    mshield_smc(MSHIELD_SMC_ROM_CTRL_LATENCY, tag_ram, data_ram);

#else /* !TI_MSHIELD */
    /* Direct register access */
    /* 1: Write to aux Tag RAM latentcy, Data RAM latency, prefect, power control registers  */
    l2cc->control.aux_control      = aux;
    l2cc->control.tag_ram_control  = tag_ram;
    l2cc->control.data_ram_control = data_ram;
    l2cc->control2.prefetch_ctrl   = prefetch;

#endif /* TI_MSHIELD */

    /* 2: Invalidate by way. */
    l2cc->maintenance.inv_way = 0xffff;
    while ( l2cc->maintenance.inv_way & MAINTENANCE_PENDING );

    /* 3: write to lockdown D & I reg9 if required  */
    if ( (l2cc->id.cache_type & PL310_LOCKDOWN_BY_MASK) == PL310_LOCKDOWN_BY_MASTER) {
        /* disable lockdown */
        l2cc->lockdown.d_lockdown0 = 0;
        l2cc->lockdown.i_lockdown0 = 0;
        l2cc->lockdown.d_lockdown1 = 0;
        l2cc->lockdown.i_lockdown1 = 0;
        l2cc->lockdown.d_lockdown2 = 0;
        l2cc->lockdown.i_lockdown2 = 0;
        l2cc->lockdown.d_lockdown3 = 0;
        l2cc->lockdown.i_lockdown3 = 0;
        l2cc->lockdown.d_lockdown4 = 0;
        l2cc->lockdown.i_lockdown4 = 0;
        l2cc->lockdown.d_lockdown5 = 0;
        l2cc->lockdown.i_lockdown5 = 0;
        l2cc->lockdown.d_lockdown6 = 0;
        l2cc->lockdown.i_lockdown6 = 0;
        l2cc->lockdown.d_lockdown7 = 0;
        l2cc->lockdown.i_lockdown7 = 0;
    }
    if ( (l2cc->id.cache_type & PL310_LOCKDOWN_BY_MASK) == PL310_LOCKDOWN_BY_LINE) {
        /* disable lockdown */
        l2cc->lockdown.lock_line_eng = 0;
    }

    /* 4: write to interrupt clear register to clear any residual raw interrupts set */
    l2cc->interrupt.int_mask  = 0x0;
    /* 5: write to interrupt mask register if you want to enable interrupts (active high) */
    l2cc->interrupt.int_clear = MASK(9);

    /* 6: Enable the L2 cache */
#ifdef TI_MSHIELD
    /* Access secure registers through Security Middleware Call */
    mshield_smc(MSHIELD_SMC_ROM_CTRL_CTRL, 1, 0);
#else /* !TI_MSHIELD */
    /* Direct register access */
    l2cc->control.control |= CTRL_CTRL_EN;
#endif /* TI_MSHIELD */

#endif /* !CONFIG_DEBUG_DISABLE_L2_CACHE */
}

static inline void L2_cacheSync(void)
{
    dmb();
    l2cc->maintenance.cache_sync = 0;
    while (l2cc->maintenance.cache_sync & MAINTENANCE_PENDING);
}

void plat_cleanCache(void)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    /* Clean by way. */
    l2cc->maintenance.clean_way = 0xffff;
    while ( l2cc->maintenance.clean_way & MAINTENANCE_PENDING );
    L2_cacheSync();
#endif /* !CONFIG_DEBUG_DISABLE_L2_CACHE */
}

void plat_cleanL2Range(paddr_t start, paddr_t end)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    /* Documentation specifies this as the only possible line size */
    assert(((l2cc->id.cache_type >> 12) & 0x3) == 0x0);

    for (start = L2_LINE_START(start);
            start != L2_LINE_START(end + L2_LINE_SIZE);
            start += L2_LINE_SIZE) {
        l2cc->maintenance.clean_pa = start;
        /* do not need to wait for every invalidate as 310 is atomic */
    }
    L2_cacheSync();
#endif /* !CONFIG_DEBUG_DISABLE_L2_CACHE */
}

void plat_invalidateL2Range(paddr_t start, paddr_t end)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    /* Documentation specifies this as the only possible line size */
    assert(((l2cc->id.cache_type >> 12) & 0x3) == 0x0);

    /* We assume that if this is a partial line that whoever is calling us
     * has already done the clean, so we just blindly invalidate all the lines */

    for (start = L2_LINE_START(start);
            start != L2_LINE_START(end + L2_LINE_SIZE);
            start += L2_LINE_SIZE) {
        l2cc->maintenance.inv_pa = start;
        /* do not need to wait for every invalidate as 310 is atomic */
    }
    L2_cacheSync();
#endif /* !CONFIG_DEBUG_DISABLE_L2_CACHE */
}

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    /* Documentation specifies this as the only possible line size */
    assert(((l2cc->id.cache_type >> 12) & 0x3) == 0x0);

    for (start = L2_LINE_START(start);
            start != L2_LINE_START(end + L2_LINE_SIZE);
            start += L2_LINE_SIZE) {
        /* Work around an errata and call the clean and invalidate separately */
        l2cc->maintenance.clean_pa = start;
        dmb();
        l2cc->maintenance.inv_pa = start;
        /* do not need to wait for every invalidate as 310 is atomic */
    }
    L2_cacheSync();
#endif /* !CONFIG_DEBUG_DISABLE_L2_CACHE */
}

