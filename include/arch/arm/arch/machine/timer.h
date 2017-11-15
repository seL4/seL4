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

#ifndef __ARCH_MACHINE_TIMER_H_
#define __ARCH_MACHINE_TIMER_H_

#include <config.h>
#include <stdint.h>
#include <mode/machine/timer.h>
#include <plat/machine/hardware.h>

#define HZ_IN_KHZ 1000llu
/* convert to khz first to avoid overflow */
#define TICKS_PER_MS (TIMER_CLOCK_HZ / HZ_IN_KHZ)
/* but multiply by timer tick ms */
#define TIMER_RELOAD    (TICKS_PER_MS * CONFIG_TIMER_TICK_MS)

#if (TIMER_RELOAD >= UINTPTR_MAX)
#error "Timer reload too high"
#endif

void initTimer(void);

#endif /* __ARCH_MACHINE_TIMER_H_ */
