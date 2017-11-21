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
#ifndef __ARCH_MACHINE_GLOBAL_TIMER_H
#define __ARCH_MACHINE_GLOBAL_TIMER_H

#include <util.h>
#include <config.h>
#include <model/statedata.h>

/* ARM Cortex-A9 MPCore global timer driver
 * Documentation for this timer is available in
 * the Cortex-A9 MPCore Technical Reference Manual
 * section "Global Timer Counter Registers"
 */
struct timer {
    uint32_t countLower;
    uint32_t countUpper;
    uint32_t control;
    uint32_t isr;
    uint32_t comparatorLower;
    uint32_t comparatorUpper;
    uint32_t autoInc;
};
typedef volatile struct timer timer_t;
extern timer_t * const globalTimer;

enum control {
    ENABLE = 0,
    COMP_ENABLE = 1,
    IRQ_ENABLE = 2,
    AUTO_INC = 3,
    RESERVED = 4,
    PRESCALER = 8,
    RESERVED_2 = 16
};

/** DONT_TRANSLATE */
static inline CONST ticks_t
getTimerPrecision(void)
{
    return usToTicks(2);
}

/** DONT_TRANSLATE */
static inline ticks_t
getCurrentTime(void)
{
    uint32_t upper, upper2, lower;

    upper = globalTimer->countUpper;
    lower = globalTimer->countLower;
    upper2 = globalTimer->countUpper;

    /* account for race: upper could have increased while we
     * read lower */
    if (upper != upper2) {
        lower = globalTimer->countLower;
    }

    return (((ticks_t) upper2 << 32llu) + (ticks_t) lower);
}

/** DONT_TRANSLATE */
static inline void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurTime);
    /* disable cmp */
    globalTimer->control &= ~(BIT(COMP_ENABLE));
    /* set low bits */
    globalTimer->comparatorLower = (uint32_t) deadline;
    /* set high bits */
    globalTimer->comparatorUpper = (uint32_t) (deadline >> 32llu);
    /* enable cmp */
    globalTimer->control |= BIT(COMP_ENABLE);
    /* if this fails PRECISION is too low */
    assert(getCurrentTime() < deadline || globalTimer->isr == 1u);
}

/** DONT_TRANSLATE */
static inline void
ackDeadlineIRQ(void)
{
    /* ack the isr */
    globalTimer->isr = 1;
}

#endif /* __ARCH_MACHINE_GLOBAL_TIMER_H */
