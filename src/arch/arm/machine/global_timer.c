/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */
/* Documentation for this timer is available in
 * the Cortex-A9 MPCore Technical Reference Manual
 *  section "Global Timer Counter Registers"
 */

#include <util.h>

static volatile struct globalTimerMap {
    uint32_t countLower;
    uint32_t countUpper;
    uint32_t control;
    uint32_t isr;
    uint32_t comparatorLower;
    uint32_t comparatorUpper;
    uint32_t autoInc;
} *globalTimer;

enum control {
    ENABLE = 0,
    COMP_ENABLE = 1,
    IRQ_ENABLE = 2,
    AUTO_INC = 3,
    RESERVED = 4,
    PRESCALER = 8,
    RESERVED_2 = 16
};

time_t CONST
getMaxTimerUs(void)
{
    return UINT64_MAX / CLK_MHZ;
}

time_t CONST
getMinTimerUs(void)
{
    return 2;
}

time_t CONST
getTimerPrecision(void)
{
    return getMinTimerUs() * CLK_MHZ;
}

time_t CONST
usToTicks(time_t us)
{
    assert(us <= getMaxTimerUs());
    assert(us > getMinTimerUs());
    return us * CLK_MHZ;
}

void
setDeadline(time_t deadline)
{
    /* if this fails the caller code is wrong */
    if (deadline < ksCurrentTime) {
        printf("deadline %llx current %llx\n", deadline, ksCurrentTime);
    }
    assert(deadline > ksCurrentTime);
    /* disable cmp */
    globalTimer->control &= ~(BIT(COMP_ENABLE));
    /* set low bits */
    globalTimer->comparatorLower = (uint32_t) deadline;
    /* set high bits */
    globalTimer->comparatorUpper = (uint32_t) (deadline >> 32llu);
    /* enable cmp */
    globalTimer->control |= BIT(COMP_ENABLE);
    /* if this fails PRECISION is too low */
    assert(getCurrentTime() < deadline || globalTimer->isr == 1);
}

void
ackDeadlineIRQ(void)
{
    /* ack the isr */
    globalTimer->isr = 1;
}

time_t
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

    return (((time_t) upper2 << 32llu) + (time_t) lower);
}

BOOT_CODE void
initTimer(void)
{
    globalTimer = (volatile struct globalTimerMap *) ARM_MP_GLOBAL_TIMER_PPTR;
    /* disable the timer */
    globalTimer->control = 0;
    /* zero the timer */
    globalTimer->countLower = 0;
    globalTimer->countUpper = 0;
    /* turn it on again, wih interrupts on, comparator register off,
     * in one-shot mode, with standard prescaler */
    globalTimer->control = (1 << ENABLE) | (1 << IRQ_ENABLE);

    /* this timer will overflow in about 1000 years */
}


