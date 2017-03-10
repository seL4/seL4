/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __IPI_H
#define __IPI_H

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <arch/smp/ipi.h>

#if CONFIG_MAX_NUM_NODES > 1
#define MAX_IPI_ARGS    3   /* Maximum number of parameters to remote function */

/* IPIs could be handled, both using hardware interrupts and software flag
 * in CLH lock. 'irqPath' is used to differentiate the caller path, i.e.
 * if it is called while waiting on the lock to handle the IRQ or not. The
 * remote call handler, would decide if 'Arch_handleIPI' should return base
 * on this value, as IRQs could be re/triggered asynchronous */
void Arch_handleIPI(irq_t irq, bool_t irqPath);

/*
 * Run a synchronous function on all cores specified by mask. Return when target cores
 * have all executed the function. Caller must hold the lock.
 *
 * @param func the function to run
 * @param data1 passed to the function as first parameter
 * @param data2 passed to the function as second parameter
 * @param mask cores to run function on
 */
void doRemoteMaskOp(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t mask);

/* Run a synchronous function on a core specified by cpu.
 *
 * @param func the function to run
 * @param data1 passed to the function as first parameter
 * @param data2 passed to the function as second parameter
 * @param cpu core to run function on
 */
static void inline doRemoteOp(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t cpu)
{
    doRemoteMaskOp(func, data1, data2, data3, BIT(cpu));
}

/* List of wrapper functions
 *
 * doRemote[Mask]Op0Arg: do remote operation without any argument
 * doRemote[Mask]Op1Arg: do remote operation with one argument
 * doRemote[Mask]Op2Arg: do remote operation with two arguments
 * These should be used in favour of directly calling 'doRemote[Mask]Op'
 * in case arguments change in future.
 *
 * @param func the function to run
 * @param data passed to the function as parameters
 * @param cpu[mask] cores to run function on
 */
static void inline doRemoteMaskOp0Arg(IpiRemoteCall_t func, word_t mask)
{
    doRemoteMaskOp(func, 0, 0, 0, mask);
}

static void inline
doRemoteMaskOp1Arg(IpiRemoteCall_t func, word_t data1, word_t mask)
{
    doRemoteMaskOp(func, data1, 0, 0, mask);
}

static void inline
doRemoteMaskOp2Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t mask)
{
    doRemoteMaskOp(func, data1, data2, 0, mask);
}

static void inline
doRemoteMaskOp3Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t mask)
{
    doRemoteMaskOp(func, data1, data2, data3, mask);
}

static void inline doRemoteOp0Arg(IpiRemoteCall_t func, word_t cpu)
{
    doRemoteOp(func, 0, 0, 0, cpu);
}

static void inline
doRemoteOp1Arg(IpiRemoteCall_t func, word_t data1, word_t cpu)
{
    doRemoteOp(func, data1, 0, 0, cpu);
}

static void inline
doRemoteOp2Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t cpu)
{
    doRemoteOp(func, data1, data2, 0, cpu);
}

/* This is asynchronous call and could be called outside the lock.
 * Returns immediately.
 *
 * @param mask cores to request rescheduling
 */
void doMaskReschedule(word_t mask);

/* Request rescheduling on a core specified by cpu.
 * Returns immediately.
 *
 * @param cpu core to reschedule
 */
static void inline doReschedule(word_t cpu)
{
    if (cpu != getCurrentCPUIndex()) {
        assert(cpu < CONFIG_MAX_NUM_NODES);
        doMaskReschedule(BIT(cpu));
    }
}

#endif /* CONFIG_MAX_NUM_NODES */
#endif /* __IPI_H */
