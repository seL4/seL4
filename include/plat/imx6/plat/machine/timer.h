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

/* see tools/reciprocal.py for calculation of this value */
#define CLK_MAGIC 2207854675llu
#define CLK_SHIFT 40llu

#define TIMER_CLOCK_HZ     498000000llu

#include <arch/machine/timer.h>

static inline CONST time_t
getKernelWcetUs(void) {
    return 10u;
}

/* we use the cortex-a9 global timer on this platform */
#include <arch/machine/global_timer.h>

compile_assert(magic_will_work, TIMER_CLOCK_MHZ == 498llu);
