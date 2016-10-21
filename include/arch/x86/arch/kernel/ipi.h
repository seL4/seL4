/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#pragma once

#include <config.h>
#include <types.h>
#include <plat/machine.h>

#if CONFIG_MAX_NUM_NODES > 1

void Arch_handleIPI(irq_t irq);

typedef enum {
    IpiRemoteCall_Null
} IpiRemoteCall_t;

/*
 * Run a synchronous function on all cores specified by mask. Return when target cores
 * have all executed the function. Caller must hold the lock.
 *
 * @param func the function to run
 * @param data passed to the function
 * @param mask cores to run function on
 */
void doRemoteMaskOp(IpiRemoteCall_t func, word_t data, word_t mask);

/* This is asynchronous call and could be called outside the lock.
 * Returns immediately.
 *
 * @param cpu core to request rescheduling
 */
void doReschedule(word_t cpu);

/* Run a synchronous function on a core specified by cpu.
 *
 * @param func the function to run
 * @param data passed to the function
 * @param cpu core to run function on
 */
static void inline FORCE_INLINE
doRemoteOp(IpiRemoteCall_t func, word_t data, word_t cpu)
{
    doRemoteMaskOp(func, data, BIT(cpu));
}

#endif
