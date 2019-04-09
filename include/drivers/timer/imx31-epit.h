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
