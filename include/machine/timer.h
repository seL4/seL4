/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/machine/timer.h>

#ifdef CONFIG_KERNEL_MCS
#include <types.h>
#include <arch/linker.h>

/* This function must be implemented by a timer driver to provide the raw system
 * time. The value returned for each call is supposed to be greater or equal
 * than the value returned by previous call and to never roll over practically.
 */
/** MODIFIES: [*] */
static inline ticks_t driver_getSystemTime(void);

/* Get the current time. */
static inline ticks_t getCurrentTime(void)
{
    time_t prev = NODE_STATE(ksPrevTime);
    /* Get current time from the timer driver. */
    uint64_t now = driver_getCurrentTime();
    /* Time must increment monotonically and never roll over, which is something
     * the timer driver and timer hardware should already guarantee. The sanity
     * check here is to catch broken configurations or implementations, but also
     * an actual roll over.
     */
    if (unlikely(now < prev)) {
        /* There are a few option here. Take the last timestamp and produce an
         * artificial time stamp from this. That would keep the system running
         * somehow even if the time is broken. But it's more likely that this
         * is a fatal situation, in the end a 64-bit timer is used because it is
         * simply big enough to practically never roll over.
         */
        printf("FATAL ERROR: timer roll over from %"PRIu64" to %"PRIu64"\n",
               prev, now);
        halt();
    }

    NODE_STATE(ksPrevTime) = now;
    return now;
}

/* set the next deadline irq - deadline is absolute */
/** MODIFIES: [*] */
static inline void setDeadline(ticks_t deadline);
/* ack previous deadline irq */
/** MODIFIES: [*] */
static inline void ackDeadlineIRQ(void);

/* get the expected wcet of the kernel for this platform */
static PURE inline ticks_t getKernelWcetTicks(void)
{
    return usToTicks(getKernelWcetUs());
}
#else /* CONFIG_KERNEL_MCS */
static inline void resetTimer(void);
#endif /* !CONFIG_KERNEL_MCS */

/* Update time on a processing node (usually a core). */
static inline void updateNodeTime(void)
{
#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
#endif
}

/* Initialize time on a processing node (usually a core). */
static inline void initNodeTime(void)
{
#ifdef CONFIG_KERNEL_MCS
    updateNodeTime();
    NODE_STATE(ksConsumed) = 0;
#endif
}
