/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <stdint.h>

/*
 * Samsung Exynos multi-core timer implementation
 * Samsung has a habit of ripping out ARM IP and
 * replacing it with their own.
 */

#define GCNTWSTAT_CNTH       (1U << 1)
#define GCNTWSTAT_CNTL       (1U << 0)

#define GTCON_EN             (1U << 8)
#define GTCON_COMP3_AUTOINC  (1U << 7)
#define GTCON_COMP3_EN       (1U << 6)
#define GTCON_COMP2_AUTOINC  (1U << 5)
#define GTCON_COMP2_EN       (1U << 4)
#define GTCON_COMP1_AUTOINC  (1U << 3)
#define GTCON_COMP1_EN       (1U << 2)
#define GTCON_COMP0_AUTOINC  (1U << 1)
#define GTCON_COMP0_EN       (1U << 0)

#define GINT_COMP3_IRQ       (1U << 3)
#define GINT_COMP2_IRQ       (1U << 2)
#define GINT_COMP1_IRQ       (1U << 1)
#define GINT_COMP0_IRQ       (1U << 0)

#define GWSTAT_TCON          (1U << 16)
#define GWSTAT_COMP3_ADD_INC (1U << 14)
#define GWSTAT_COMP3H        (1U << 13)
#define GWSTAT_COMP3L        (1U << 12)
#define GWSTAT_COMP2_ADD_INC (1U << 10)
#define GWSTAT_COMP2H        (1U << 9)
#define GWSTAT_COMP2L        (1U << 8)
#define GWSTAT_COMP1_ADD_INC (1U << 6)
#define GWSTAT_COMP1H        (1U << 5)
#define GWSTAT_COMP1L        (1U << 4)
#define GWSTAT_COMP0_ADD_INC (1U << 2)
#define GWSTAT_COMP0H        (1U << 1)
#define GWSTAT_COMP0L        (1U << 0)

#define LTCON_XXX0           (1U << 3)
#define LTCON_INTERVAL_MODE  (1U << 2)
#define LTCON_IEN            (1U << 1)
#define LTCON_EN             (1U << 0)

#define LTWSTAT_TCON         (1U << 3)
#define LTWSTAT_TCOMP        (1U << 1)
#define LTWSTAT_TCNT         (1U << 0)



struct mct_global_map {
    uint32_t reserved0[64];
    uint32_t cntl;           /* 0x100 Low word of count */
    uint32_t cnth;           /* 0x104 High word of count */
    uint32_t reserved1[1];
    uint32_t cnt_wstat;      /* 0x110 Write status for cnt */
    uint32_t reserved2[60];

    uint32_t comp0l;         /* 0x200 Low word of Compare value */
    uint32_t comp0h;         /* 0x204 High word of Compare value*/
    uint32_t comp0_add_inc;  /* 0x208 Low word of Automatic increment amount */
    uint32_t comp0_res;

    uint32_t comp1l;         /* 0x210 Low word of Compare value */
    uint32_t comp1h;         /* 0x214 High word of Compare value*/
    uint32_t comp1_add_inc;  /* 0x218 Low word of Automatic increment amount */
    uint32_t comp1_res;

    uint32_t comp2l;         /* 0x220 Low word of Compare value */
    uint32_t comp2h;         /* 0x224 High word of Compare value*/
    uint32_t comp2_add_inc;  /* 0x228 Low word of Automatic increment amount */
    uint32_t comp2_res;

    uint32_t comp3l;         /* 0x230 Low word of Compare value */
    uint32_t comp3h;         /* 0x234 High word of Compare value*/
    uint32_t comp3_add_inc;  /* 0x238 Low word of Automatic increment amount */
    uint32_t comp3_res;

    uint32_t tcon;           /* 0x240 Timer control */
    uint32_t int_stat;       /* 0x244 Interrupt pending status */
    uint32_t int_en;         /* 0x248 Interrupt enable */
    uint32_t wstat;          /* 0x24C  write status */
    uint32_t reserved3[44];
};

struct mct_local_map {
    uint32_t tcompl;         /* 0x00 */
    uint32_t tcntl;          /* 0x04 */
    uint32_t tcomph;         /* 0x08 */
    uint32_t tcnth;          /* 0x0C */
    uint32_t reserved0[4];
    uint32_t tcon;           /* 0x20 Timer control */
    uint32_t int_stat;       /* 0x30 Interrupt status */
    uint32_t int_en;         /* 0x34 Interrupt enable */
    uint32_t reserved1[2];
    uint32_t wstat;          /* 0x40 Write status */
    uint32_t reserved2[50];
};

struct mct_map {
    struct mct_global_map global;
    struct mct_local_map local[4];
};
typedef volatile struct mct_map timer_t;
extern timer_t *mct;

static inline void mct_reset(void)
{
    mct->global.int_stat = mct->global.int_stat;
}

static inline void mct_clear_write_status(void)
{
    /* Clear write status */
    mct->global.wstat = mct->global.wstat;
    mct->global.cnt_wstat = mct->global.cnt_wstat;
}


