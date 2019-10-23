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

#ifndef __DRIVER_TIMER_MCT_EXYNOS4412_H
#define __DRIVER_TIMER_MCT_EXYNOS4412_H

#include <drivers/timer/mct.h>

static inline void resetTimer(void)
{
    mct_reset();
}

#ifdef CONFIG_KERNEL_MCS
/** DONT_TRANSLATE **/
static inline ticks_t getCurrentTime(void)
{
    uint32_t hi, hi2, lo;
    hi2 = mct->global.cnth;

    do {
        hi = hi2;
        lo = mct->global.cntl;
        hi2 = mct->global.cnth;
    } while (hi != hi2);

    return ((((uint64_t) hi) << 32llu) | (uint64_t) lo);
}

/** DONT_TRANSLATE **/
static inline void setDeadline(ticks_t deadline)
{
    /*
     * After writing a value to a comp register a bit in the wstat
     * register is asserted. A write of 1 clears this bit.
     */
    mct->global.comp0h = (uint32_t)(deadline >> 32u);
    while (!(mct->global.wstat & GWSTAT_COMP0H));
    mct->global.wstat |= GWSTAT_COMP0H;

    mct->global.comp0l = (uint32_t) deadline;

    while (!(mct->global.wstat & GWSTAT_COMP0L));
    mct->global.wstat |= GWSTAT_COMP0L;
}

static inline void ackDeadlineIRQ(void)
{
    /* ack everything */
    mct_reset();
}

#endif

#endif /* !__DRIVER_TIMER_MCT_EXYNOS4412_H */
