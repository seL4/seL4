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

/* convert to khz first to avoid overflow */
#define TIMER_CLOCK_KHZ (TIMER_CLOCK_HZ  / HZ_IN_KHZ)
#define TIMER_CLOCK_MHZ (TIMER_CLOCK_HZ /  HZ_IN_MHZ)

/* all platforms define their timer functions here */
#include <plat/machine/timer.h>

static PURE inline ticks_t
getKernelWcetTicks(void)
{
    return usToTicks(getKernelWcetUs());
}

#endif
