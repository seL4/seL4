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

#include <config.h>
#include <basic_types.h>
#include <arch/machine/global_timer.h>

/* see tools/reciprocal.py for calculation of this value */
#define CLK_MAGIC 4415709349llu
#define CLK_SHIFT 41llu

#define TIMER_CLOCK_HZ     498000000llu

static inline CONST time_t
getKernelWcetUs(void)
{
    return 10u;
}

compile_assert(magic_will_work, TIMER_CLOCK_MHZ == 498llu)
#endif /* !__PLAT_MACHINE_TIMER_H */
