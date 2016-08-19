/*
 * Copyright 2016, Data61 CSIRO
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#pragma once

#include <config.h>
#include <basic_types.h>
#include <arch/linker.h>

/* see tools/reciprocal.py for calculation of CLK_MAGIC and CLK_SHIFT */
#define CLK_MAGIC 2863311531
#define CLK_SHIFT 35
#define TIMER_CLOCK_HZ     12000000llu

#include <arch/machine/timer.h>
#include <arch/machine/generic_timer.h>

compile_assert(magic_will_work, TIMER_CLOCK_MHZ == 12llu);

static inline PURE ticks_t
getTimerPrecision(void)
{
    return TIMER_CLOCK_HZ / HZ_IN_MHZ;
}

CONST static inline time_t
getKernelWcetUs(void)
{
    return 10u;
}

static inline BOOT_CODE void
initTimer(void)
{
    initGenericTimer();
}
