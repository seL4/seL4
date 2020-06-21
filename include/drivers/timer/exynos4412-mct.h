/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

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


