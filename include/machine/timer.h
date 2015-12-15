/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __TIMER_H
#define __TIMER_H

#define US_PER_MS 1000llu

/* max amount of time this timer can represent */
PURE time_t getMaxTimerUs(void);
/* amount of time to set interrupts early by to make sure we deal with them on time */
PURE ticks_t getTimerPrecision(void);
CONST time_t getKernelWcetUs(void);
PURE ticks_t usToTicks(time_t us);
PURE time_t ticksToUs(ticks_t ticks);

/** MODIFIES: [*] */
void initTimer(void);
/** MODIFIES: [*] */
void setDeadline(ticks_t deadline);
/** MODIFIES: [*] */
ticks_t getCurrentTime(void);
/** MODIFIES: [*] */
void ackDeadlineIRQ(void);

static PURE inline ticks_t
getKernelWcetTicks(void)
{
    return usToTicks(getKernelWcetUs());
}

#endif

