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

#ifndef __ARCH_MACHINE_GENERIC_TIMER_H_
#define __ARCH_MACHINE_GENERIC_TIMER_H_

#include <config.h>
#include <stdint.h>
#include <api/types.h>
#include <mode/machine/timer.h>
#include <mode/machine.h>
#include <model/statedata.h>

/* ARM generic timer implementation */

/** DONT_TRANSLATE **/
static inline ticks_t
getCurrentTime(void)
{
    ticks_t time;
    SYSTEM_READ_64(CNT_CT, time);
    return time;
}

/** DONT_TRANSLATE **/
static inline void
setDeadline(ticks_t deadline)
{
    assert(deadline >= ksCurTime);
    SYSTEM_WRITE_64(CNT_CVAL, deadline);
}

static inline void
ackDeadlineIRQ(void)
{
    ticks_t deadline = UINT64_MAX;
    setDeadline(deadline);
}

BOOT_CODE void initGenericTimer(void);
#endif /* __ARCH_MACHINE_GENERIC_TIMER_H */
