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

#define TIMER_CLOCK_HZ 32768llu // 32KHz
/* see tools/reciprocal.py for calculation of this value */
#define CLK_MAGIC 1
#define CLK_SHIFT 5
compile_assert(magic_will_work, TIMER_CLOCK_KHZ == 32llu)

/* Memory map for EPIT (Enhanced Periodic Interrupt Timer). */
struct timer {
    uint32_t epitcr;
    uint32_t epitsr;
    uint32_t epitlr;
    uint32_t epitcmpr;
    uint32_t epitcnt;
};
typedef volatile struct timer timer_t;
extern timer_t *epit1;

static inline CONST time_t
getKernelWcetUs(void)
{
    fail("Not implemented");
}

static inline CONST ticks_t
getTimerPrecision(void)
{
    fail("Not implemented");
    return 0llu;
}

static inline ticks_t
getCurrentTime(void) {
    fail("Not implemented");
    return 0llu;
}

static inline void
setDeadline(ticks_t deadline) {
    fail("Not implemented");
}

static inline void
ackDeadlineIRQ(void) {
    fail("Not implemented");
}

#endif /* !__PLAT_MACHINE_TIMER_H */
