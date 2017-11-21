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

#ifndef __PLAT_MACHINE_TIMER_H
#define __PLAT_MACHINE_TIMER_H

#define TIMER_CLOCK_HZ 7000000llu
/* see tools/reciprocal.py for calculation of this value */
#define CLK_MAGIC 4908534053
#define CLK_SHIFT 35
compile_assert(magic_will_work, TIMER_CLOCK_MHZ == 7llu)

#include <config.h>
#include <basic_types.h>
#include <arch/linker.h>

static inline CONST time_t
getKernelWcetUs(void)
{
    fail("Not implemented");
}

static inline CONST ticks_t
getTimerPrecision(void)
{
    fail("Not implemented");
    return 0llu;
}

static inline ticks_t
getCurrentTime(void) {
    fail("Not implemented");
    return 0llu;
}

static inline void
setDeadline(ticks_t deadline) {
    fail("Not implemented");
}

static inline void
ackDeadlineIRQ(void) {
    fail("Not implemented");
}

#endif /* !__PLAT_MACHINE_TIMER_H */
