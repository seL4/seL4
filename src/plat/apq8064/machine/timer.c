/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */
/* @AUTHOR(akroh@ertos.nicta.com.au) */

#include <plat/machine/hardware.h>

#define TCXO_CLK_MHZ         7UL /* empirically */
#define TIMER_FIN_MHZ        TCXO_CLK_MHZ

#define DGT_TIMER_PPTR (TIMER_PPTR + 0x024)

struct dgt_timer {
    uint32_t mtch;        /* +0x024 */
    uint32_t cnt;         /* +0x028 */
    uint32_t en;          /* +0x02C */
    uint32_t clr;         /* +0x030 */
    uint32_t clk_ctl;     /* +0x034 */
};
typedef volatile struct dgt_timer timer_t;

timer_t* dgt_tmr = (timer_t*)DGT_TIMER_PPTR;

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

#define TIMER_INTERVAL_US  (CONFIG_TIMER_TICK_MS * 1000)
#define TIMER_BITS         32


#define TIMER_INTERVAL_TICKS  (TIMER_FIN_MHZ * TIMER_INTERVAL_US / PRESCALE_VAL)

#define TIMER_MATCH_VAL       (TIMER_INTERVAL_TICKS)

/* sanity check */
#if TIMER_MATCH_VAL >= (1UL << TIMER_BITS)
#error Timer match value overflow
#endif
#if TIMER_MATCH_VAL <= 0
#error Timer match value underflow
#endif



/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    /* Stop the timer */
    dgt_tmr->en = 0;
    dgt_tmr->clr = 0xC0FFEE;
    /* Configure the timer */
    dgt_tmr->mtch = TIMER_MATCH_VAL;
    /* Start the timer */
    dgt_tmr->en = DGTTMR_EN_CLR_ON_MTCH_EN | DGTTMR_EN_EN;
}

/**
   DONT_TRANSLATE
 */
void
resetTimer(void)
{
    /* Nothing to do */
}


