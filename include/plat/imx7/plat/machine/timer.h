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

#define TIMER_CLOCK_HZ 8000000llu
#define CLK_MAGIC 1llu
#define CLK_SHIFT 8llu

/* see tools/reciprocal.py for calculation of CLK_MAGIC and CLK_SHIFT */
compile_assert(magic_will_work, TIMER_CLOCK_MHZ == 8llu);
#include <config.h>
#include <basic_types.h>
#include <arch/linker.h>
#include <arch/machine/generic_timer.h>

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
