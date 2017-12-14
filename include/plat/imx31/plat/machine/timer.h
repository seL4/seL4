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

#define TIMER_CLOCK_HZ 18600000 // 18.6MHz -- calculated by trial and error, roughly precise
/* see tools/reciprocal.py for calculation of this value */
#define CLK_MAGIC 7566531633
#define CLK_SHIFT 47
compile_assert(magic_will_work, TIMER_CLOCK_KHZ == 18600llu)

/* gptir and gptsr bits */
#define OF1IE 0 /* output compare 1 */
#define ROV   5 /* roll over */

/* Memory map for GPT (General Purpose Timer). */
struct timer {
    uint32_t gptcr; /* control */
    uint32_t gptpr; /* prescaler */
    uint32_t gptsr; /* status register */
    uint32_t gptir; /* interrupt register */
    uint32_t gptcr1;
    uint32_t gptcr2;
    uint32_t gptcr3;
    uint32_t gpticr1;
    uint32_t gpticr2;
    uint32_t gptcnt;
};
typedef volatile struct timer timer_t;
extern timer_t *gpt;
extern ticks_t high_bits;

static inline CONST time_t
getKernelWcetUs(void)
{
    return 10;
}

static inline CONST ticks_t
getTimerPrecision(void)
{
    return 0;
}

static inline ticks_t
getCurrentTime(void)
{
    return ((high_bits + !!(gpt->gptsr & BIT(ROV))) << 32llu) + gpt->gptcnt;
}

static inline void
setDeadline(ticks_t deadline)
{
    if (((uint32_t) deadline) > gpt->gptcnt) {
        /* turn on compare irq */
        gpt->gptir |= BIT(OF1IE);
        /* set the deadline */
        do {
            gpt->gptcr1 = (uint32_t) deadline;
        } while (gpt->gptcr1 != (uint32_t) deadline);
    }
}

static inline void
ackDeadlineIRQ(void)
{
    if (gpt->gptsr & BIT(ROV)) {
        high_bits++;
    }

    /* turn off compare irq */
    gpt->gptir &= ~(BIT(OF1IE));
    /* ack either irq */
    gpt->gptsr |= (BIT(OF1IE) | BIT(ROV));
}

#endif /* !__PLAT_MACHINE_TIMER_H */
