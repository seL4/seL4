/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define TISR_MATCH_FLAG     BIT(0)
#define TISR_OVF_FLAG       BIT(1)
#define TISR_TCAR_FLAG      BIT(2)

/* Kernel uses DMTIMER4 */
struct timer {
    uint32_t tidr; // 00h TIDR Identification Register
    uint32_t padding1[3];
    uint32_t cfg; // 10h TIOCP_CFG Timer OCP Configuration Register
    uint32_t padding2[3];
    uint32_t tieoi; // 20h IRQ_EOI Timer IRQ End-Of-Interrupt Register
    uint32_t tisrr; // 24h IRQSTATUS_RAW Timer IRQSTATUS Raw Register
    uint32_t tisr; // 28h IRQSTATUS Timer IRQSTATUS Register
    uint32_t tier; // 2Ch IRQSTATUS_SET Timer IRQENABLE Set Register
    uint32_t ticr; // 30h IRQSTATUS_CLR Timer IRQENABLE Clear Register
    uint32_t twer; // 34h IRQWAKEEN Timer IRQ Wakeup Enable Register
    uint32_t tclr; // 38h TCLR Timer Control Register
    uint32_t tcrr; // 3Ch TCRR Timer Counter Register
    uint32_t tldr; // 40h TLDR Timer Load Register
    uint32_t ttgr; // 44h TTGR Timer Trigger Register
    uint32_t twps; // 48h TWPS Timer Write Posted Status Register
    uint32_t tmar; // 4Ch TMAR Timer Match Register
    uint32_t tcar1; // 50h TCAR1 Timer Capture Register
    uint32_t tsicr; // 54h TSICR Timer Synchronous Interface Control Register
    uint32_t tcar2; // 58h TCAR2 Timer Capture Register
};

typedef volatile struct timer timer_t;
extern timer_t *timer;

#ifdef CONFIG_KERNEL_MCS

extern uint32_t high_bits;

/* Read the current time from the timer. */
static inline ticks_t getCurrentTime(void)
{
    bool_t overflow = !!(timer->tisr & TISR_OVF_FLAG);
    return ((((uint64_t) high_bits + overflow) << 32llu) + timer->tcrr);
}

#define HIGH_BYTES 0xffffffff00000000

bool_t high_deadline = false;
/* set the next deadline irq - deadline is absolute */
static inline void setDeadline(ticks_t deadline)
{
    /* Set the deadline in two parts */
    if ((deadline & HIGH_BYTES) != 0) {
        deadline = (deadline & HIGH_BYTES) >> 32;
        high_deadline = true;
    }
    assert((deadline & HIGH_BYTES) == 0);
    timer->tmar = (uint32_t)deadline;
}

/* ack previous deadline irq */
static inline void ackDeadlineIRQ(void)
{
    /* check if this is an overflow or match irq and ack */
    if (timer->tisr & TISR_OVF_FLAG) {
        high_bits++;
        timer->tisr = TISR_OVF_FLAG;
        assert((timer->tisr & TISR_OVF_FLAG) == 0);

    }
    if (timer->tisr & TISR_MATCH_FLAG) {
        if (high_deadline) {
            timer->tmar = 0xffffffff;
            high_deadline = false;
        }
        timer->tisr = TISR_MATCH_FLAG;
        assert((timer->tisr & TISR_MATCH_FLAG) == 0);
    }
}
#else /* CONFIG_KERNEL_MCS */

static inline void resetTimer(void)
{
    timer->tisr = TISR_OVF_FLAG | TISR_MATCH_FLAG | TISR_TCAR_FLAG;
    ackInterrupt(KERNEL_TIMER_IRQ);
}
#endif /* !CONFIG_KERNEL_MCS */

