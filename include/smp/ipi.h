/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <arch/smp/ipi.h>

#ifdef ENABLE_SMP_SUPPORT
#define MAX_IPI_ARGS    3   /* Maximum number of parameters to remote function */

static volatile struct {
    word_t count;
    word_t globalsense;

    PAD_TO_NEXT_CACHE_LN(sizeof(word_t) + sizeof(word_t));
} ipiSyncBarrier = {0};                  /* IPI barrier for remote call synchronization */

static volatile word_t totalCoreBarrier; /* number of cores involved in IPI 'in progress' */
static word_t ipi_args[MAX_IPI_ARGS];    /* data to be passed to the remote call function */

static inline word_t get_ipi_arg(word_t n)
{
    assert(n < MAX_IPI_ARGS);
    return ipi_args[n];
}

static inline void ipi_wait(word_t cores)
{
    word_t localsense = ipiSyncBarrier.globalsense;

    if (__atomic_fetch_add(&ipiSyncBarrier.count, 1, __ATOMIC_ACQ_REL) == cores) {
        ipiSyncBarrier.count = 0;
        ipiSyncBarrier.globalsense =
            ~ipiSyncBarrier.globalsense;
    }

    while (localsense == ipiSyncBarrier.globalsense) {
        arch_pause();
    }
}

/* Architecture independent function for sending handling pre-hardware-send IPIs */
void generic_ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking);

/* An architecture/platform should implement this function either as a wrapper to
 * its own arch_ipi_send_mask() or using the generic_ipi_send_mask() function
 * provided to be architecture agnostic.
 */
void ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking);


/* This function switches the core it is called on to the idle thread,
 * in order to avoid IPI storms. If the core is waiting on the lock, the actual
 * switch will not occur until the core attempts to obtain the lock, at which
 * point the core will capture the pending IPI, which is discarded.

 * The core who triggered the store is responsible for triggering a reschedule,
 * or this call will idle forever */
void ipiStallCoreCallback(bool_t irqPath);

/* IPIs could be handled, both using hardware interrupts and software flag
 * in CLH lock. 'irqPath' is used to differentiate the caller path, i.e.
 * if it is called while waiting on the lock to handle the IRQ or not. The
 * remote call handler, would decide if 'handleIPI' should return base
 * on this value, as IRQs could be re/triggered asynchronous */
void handleIPI(irq_t irq, bool_t irqPath);

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

static void inline doRemoteMaskOp1Arg(IpiRemoteCall_t func, word_t data1, word_t mask)
{
    doRemoteMaskOp(func, data1, 0, 0, mask);
}

static void inline doRemoteMaskOp2Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t mask)
{
    doRemoteMaskOp(func, data1, data2, 0, mask);
}

static void inline doRemoteMaskOp3Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t mask)
{
    doRemoteMaskOp(func, data1, data2, data3, mask);
}

static void inline doRemoteOp0Arg(IpiRemoteCall_t func, word_t cpu)
{
    doRemoteOp(func, 0, 0, 0, cpu);
}

static void inline doRemoteOp1Arg(IpiRemoteCall_t func, word_t data1, word_t cpu)
{
    doRemoteOp(func, data1, 0, 0, cpu);
}

static void inline doRemoteOp2Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t cpu)
{
    doRemoteOp(func, data1, data2, 0, cpu);
}

static void inline doRemoteOp3Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t cpu)
{
    doRemoteOp(func, data1, data2, data3, cpu);
}

/* This is asynchronous call and could be called outside the lock.
 * Returns immediately.
 *
 * @param mask cores to request rescheduling
 */
void doMaskReschedule(word_t mask);



#endif /* ENABLE_SMP_SUPPORT */

#ifdef CONFIG_DEBUG_BUILD
exception_t handle_SysDebugSendIPI(void);
#endif
/* Hardware implementation for sending IPIs */
void ipi_send_target(irq_t irq, word_t cpuTargetList);
