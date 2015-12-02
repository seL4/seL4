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

time_t getMaxTimerUs(void);
time_t getMinTimerUs(void);
time_t getTimerPrecision(void);
time_t usToTicks(time_t us);
time_t ticksToUs(time_t ticks);

/** MODIFIES: [*] */
void initTimer(void);
/** MODIFIES: [*] */
void setDeadline(time_t deadline);
/** MODIFIES: [*] */
time_t getCurrentTime(void);
/** MODIFIES: [*] */
void ackDeadlineIRQ(void);

#endif

