/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __DRIVERS_TIMER_IMX31_EPIT_H
#define __DRIVERS_TIMER_IMX31_EPIT_H

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

static inline void resetTimer(void)
{
    epit1->epitsr = 1;
    /* Timer resets automatically */
}

#endif /* !__DRIVERS_TIMER_IMX31_EPIT_H */
