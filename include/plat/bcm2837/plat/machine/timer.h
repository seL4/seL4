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

#define TIMER_CLOCK_HZ 19200000llu
#define CLK_MAGIC 458129845
#define CLK_SHIFT 43

/* see tools/reciprocal.py for calculation of CLK_MAGIC and CLK_SHIFT */
compile_assert(magic_will_work, TIMER_CLOCK_KHZ == 19200llu);

#include <arch/machine/generic_timer.h>
#include <config.h>
#include <basic_types.h>
#include <arch/linker.h>

static inline CONST time_t
getKernelWcetUs(void)
{
    return 10u;
}

static inline CONST ticks_t
getTimerPrecision(void)
{
    return 0llu;
}

#endif /* !__PLAT_MACHINE_TIMER_H */
