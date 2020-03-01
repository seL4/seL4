/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/* @AUTHOR(akroh@ertos.nicta.com.au) */

#include <plat/machine/hardware.h>

#define DGT_TIMER_PPTR (TIMER_PPTR + 0x024)

struct timer {
    uint32_t mtch;        /* +0x024 */
    uint32_t cnt;         /* +0x028 */
    uint32_t en;          /* +0x02C */
    uint32_t clr;         /* +0x030 */
    uint32_t clk_ctl;     /* +0x034 */
};
typedef volatile struct timer timer_t;
timer_t *dgt_tmr = (timer_t *) DGT_TIMER_PPTR;

#define DGTTMR_EN_CLR_ON_MTCH_EN    (1U << 1)
#define DGTTMR_EN_EN                (1U << 0)
#define DGTTMR_CLK_CTRL(x)          ((x) << 0)
#define DGTTMR_CLK_CTRL_DIV1        DGTTMR_CLK_CTRL(0x0)
#define DGTTMR_CLK_CTRL_DIV2        DGTTMR_CLK_CTRL(0x1)
#define DGTTMR_CLK_CTRL_DIV3        DGTTMR_CLK_CTRL(0x2)
#define DGTTMR_CLK_CTRL_DIV4        DGTTMR_CLK_CTRL(0x3)
#define DGTTMR_CLK_CTRL_MASK        DGTTMR_CLK_CTRL(0x3)

#define PRESCALER          DGTTMR_CLK_CTRL_DIV1
#define PRESCALE_VAL       1

#define TIMER_MATCH_VAL  (TIMER_RELOAD / PRESCALE_VAL)

BOOT_CODE void initTimer(void)
{
    /* Stop the timer */
    dgt_tmr->en = 0;
    dgt_tmr->clr = 0xC0FFEE;
    /* Configure the timer */
    dgt_tmr->mtch = TIMER_MATCH_VAL;
    /* Start the timer */
    dgt_tmr->en = DGTTMR_EN_CLR_ON_MTCH_EN | DGTTMR_EN_EN;
}
