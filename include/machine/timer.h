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

#include <util.h>
#include <arch/linker.h>
#include <model/statedata.h>
#include <arch/machine/timer.h>

/* get the expected wcet of the kernel for this platform */
static inline PURE time_t getKernelWcetTicks(void);
/* Read the current time from the timer. */
static inline ticks_t getCurrentTime(void);
/* set the next deadline irq - deadline is absolute */
static inline void setDeadline(ticks_t deadline);
/* ack previous deadline irq */
static inline void ackDeadlineIRQ(void);
/* initialise the timer */
BOOT_CODE void initTimer(void);

static PURE inline ticks_t
getKernelWcetTicks(void)
{
    return usToTicks(getKernelWcetUs());
}

#endif
