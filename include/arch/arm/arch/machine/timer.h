/*
 * Copyright 2016, Data 61, CSIRO
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */
#pragma once

#include <config.h>
#include <plat/machine/hardware.h>
#include <mode/machine.h>

#define HZ_IN_KHZ 1000llu
/* convert to khz first to avoid overflow */
#define TICKS_PER_MS (TIMER_CLOCK_HZ / HZ_IN_KHZ)
/* but multiply by timer tick ms */
#define TIMER_RELOAD    (TICKS_PER_MS * CONFIG_TIMER_TICK_MS)

/* ARM generic timer implementation */

/* Use Hypervisor Physical timer */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define CNT_TVAL CNTHP_TVAL
#define CNT_CTL  CNTHP_CTL
#define CNT_CVAL CNTHP_CVAL
#else
/* Use virtual timer */
#define CNT_TVAL CNTV_TVAL
#define CNT_CTL  CNTV_CTL
#define CNT_CVAL CNTV_CVAL
#endif

/**
  DONT_TRANSLATE
  */
static inline void
resetGenericTimer(void)
{
    MCR(CNT_TVAL, TIMER_RELOAD);
    MCR(CNT_CTL, BIT(0));
}

/**
 DONT_TRANSLATE
 */
BOOT_CODE static inline void
initGenericTimer(void)
{
    if (config_set(CONFIG_DEBUG_BUILD)) {
        /* check the frequency is correct */
        uint32_t gpt_cnt_tval;
        MRC(CNTFRQ, gpt_cnt_tval);
        if (gpt_cnt_tval != 0 && gpt_cnt_tval != TIMER_CLOCK_HZ) {
            printf("gpt_cnt_tval %u, expected %u\n", gpt_cnt_tval, (uint32_t) TIMER_CLOCK_HZ);
            /* it's ok to call halt here as it's a debug build */
            halt();
        }
    }

    resetGenericTimer();
}
