/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __PLAT_MACHINE_TIMER_H
#define __PLAT_MACHINE_TIMER_H

#include <config.h>
#include <basic_types.h>
#include <arch/linker.h>
#include <plat/machine/interrupt.h>

#define TIMER_CLOCK_HZ 13000000llu
#define TISR_OVF_FLAG       BIT(1)
#define TISR_MATCH_FLAG     BIT(0)

/* see tools/reciprocal.py for calculation of this value */
#define CLK_MAGIC 1321528399llu
#define CLK_SHIFT 34u

struct timer {
    uint32_t tidr;   /* GPTIMER_TIDR 0x00 */
    uint32_t padding1[3];
    uint32_t cfg;    /* GPTIMER_CFG 0x10 */
    uint32_t tistat; /* GPTIMER_TISTAT 0x14 */
    uint32_t tisr;   /* GPTIMER_TISR 0x18 */
    uint32_t tier;   /* GPTIMER_TIER 0x1C */
    uint32_t twer;   /* GPTIMER_TWER 0x20 */
    uint32_t tclr;   /* GPTIMER_TCLR 0x24 */
    uint32_t tcrr;   /* GPTIMER_TCRR 0x28 */
    uint32_t tldr;   /* GPTIMER_TLDR 0x2C */
    uint32_t ttgr;   /* GPTIMER_TTGR 0x30 */
    uint32_t twps;   /* GPTIMER_TWPS 0x34 */
    uint32_t tmar;   /* GPTIMER_TMAR 0x38 */
    uint32_t tcar1;  /* GPTIMER_TCAR1 0x3C */
    uint32_t tsicr;  /* GPTIMER_TSICR 0x40 */
    uint32_t tcar2;  /* GPTIMER_TCAR2 0x44 */
    uint32_t tpir;   /* GPTIMER_TPIR 0x48 */
    uint32_t tnir;   /* GPTIMER_TNIR 0x4C */
    uint32_t tcvr;   /* GPTIMER_TCVR 0x50 */
    uint32_t tocr;   /* GPTIMER_TOCR 0x54 */
    uint32_t towr;   /* GPTIMER_TOWR 0x58 */
};
typedef volatile struct timer timer_t;
extern timer_t *timer;
/* this is a 32-bit timer, track high_bits here */
extern uint32_t high_bits;

/** DONT_TRANSLATE */
static inline void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurTime);
    timer->tmar = (uint32_t) deadline;
}

/** DONT_TRANSLATE */
static inline ticks_t
getCurrentTime(void)
{
    bool_t overflow = !!(timer->tisr & TISR_OVF_FLAG);
    return (((uint64_t) high_bits + overflow) << 32llu) + timer->tcrr;
}

static inline CONST time_t
getKernelWcetUs(void)
{
    return 10u;
}

/** DONT_TRANSLATE */
static inline void
ackDeadlineIRQ(void)
{
    /* check if this is an overflow irq */
    if (timer->tisr & TISR_OVF_FLAG) {
        high_bits++;
    }

    /* ack everything */
    timer->tisr = TISR_OVF_FLAG | TISR_MATCH_FLAG;
    assert((timer->tisr & TISR_OVF_FLAG) == 0);
}

static inline PURE ticks_t
getTimerPrecision(void)
{
    return usToTicks(2u);
}
#endif /* !__PLAT_MACHINE_TIMER_H */
