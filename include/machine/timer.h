/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/machine/timer.h>

#ifdef CONFIG_KERNEL_MCS
#include <types.h>
#include <arch/linker.h>

/* Read the current time from the timer. */
/** MODIFIES: [*] */
static inline ticks_t getCurrentTime(void);
/* set the next deadline irq - deadline is absolute */
/** MODIFIES: [*] */
static inline void setDeadline(ticks_t deadline);
/* ack previous deadline irq */
/** MODIFIES: [*] */
static inline void ackDeadlineIRQ(void);

/* get the expected wcet of the kernel for this platform */
static PURE inline ticks_t getKernelWcetTicks(void)
{
    return usToTicks(getKernelWcetUs());
}
#else /* CONFIG_KERNEL_MCS */
static inline void resetTimer(void);
#endif /* !CONFIG_KERNEL_MCS */

/* Update time on a processing node (usually a core). */
static inline void updateNodeTime(void)
{
#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurTime) = getCurrentTime();
#endif
}

/* Initialize time on a processing node (usually a core). */
static inline void initNodeTime(void)
{
#ifdef CONFIG_KERNEL_MCS
    updateNodeTime();
    NODE_STATE(ksConsumed) = 0;
#endif
}
