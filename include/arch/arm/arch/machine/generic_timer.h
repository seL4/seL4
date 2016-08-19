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
#include <model/statedata.h>

/* ARM generic timer implementation - CPUs that implement the arm generic timers should use the following */
/* Choose generic timer to use - for platforms that support the arm generic timers */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
/* Use Hypervisor Physical timer */
#define CNT_TVAL CNTHP_TVAL
#define CNT_CTL  CNTHP_CTL
#define CNT_CVAL CNTHP_CVAL
#define CNT_CT   CNTPCT
#else
/* Use virtual timer */
#define CNT_TVAL CNTV_TVAL
#define CNT_CTL  CNTV_CTL
#define CNT_CVAL CNTV_CVAL
#define CNT_CT   CNTVCT
#endif

/** DONT_TRANSLATE **/
static inline ticks_t
getCurrentTime(void)
{
    ticks_t time;
    MRRC(CNT_CT, time);
    return time;
}

/** DONT_TRANSLATE **/
static inline void
setDeadline(ticks_t deadline)
{
    assert(deadline >= ksCurrentTime);
    MCRR(CNTV_CVAL, deadline);
    assert(deadline >= getCurrentTime());
}

static inline void
ackDeadlineIRQ(void)
{
    setDeadline(UINT64_MAX);
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

    setDeadline(UINT64_MAX);
    /* enable the timer */
    MCR(CNT_CTL, BIT(0));
}
