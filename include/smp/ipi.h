/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifndef GENMC_TEST
#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <arch/smp/ipi.h>
#endif

#ifdef ENABLE_SMP_SUPPORT
#define MAX_IPI_ARGS    3   /* Maximum number of parameters to remote function */

typedef struct {
    word_t count;               /* IPI barrier for remote call synchronisation. */
    IpiRemoteCall_t call;       /* The remote call being requested. */
    word_t args[MAX_IPI_ARGS];  /* Data to be passed to the remote call function. */
    bool_t retry;               /* Set by remote core if it can't handle the call. */
} ipi_state_t;

/* Wait for all remote callee cores to finish: */
void ipi_wait(void);

/* Architecture independent function for sending handling pre-hardware-send IPIs */
void generic_ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking);

/* An architecture/platform should implement this function either as a wrapper to
 * its own arch_ipi_send_mask() or using the generic_ipi_send_mask() function
 * provided to be architecture agnostic.
 */
void arch_ipi_send_mask(irq_t ipi, word_t mask, bool_t isBlocking);

/* Hardware implementation for sending IPIs */
void arch_ipi_send_target(irq_t irq, word_t cpuTargetList);

/* An architecture/platform should implement this function */
void arch_handleRemoteCall(IpiRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2);

/*
 * This function switches the current task to the idle thread, so that
 * the remote core can read and modify that task's state safely.
 */
void ipiStallCoreCallback(void);

/*
 * Handle blocking remote calls. `irqPath` tells whether the kernel
 * entry was because of an IRQ or because of a syscall or fault.
 */
void ipi_handle_call(bool_t irqPath);

/* Handle non-blocking remote reschedule requests. */
void handleIPI(irq_t irq);

/*
 * Run a synchronous function on all cores specified by mask. Return when target cores
 * have all executed the function. Caller must hold the lock.
 *
 * @param func the function to run
 * @param data1 passed to the function as first parameter
 * @param data2 passed to the function as second parameter
 * @param data3 passed to the function as third parameter
 * @param mask cores to run function on
 */
void ipi_remote_call(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t mask);

/* Run a synchronous function on a core specified by cpu.
 *
 * @param func the function to run
 * @param data1 passed to the function as first parameter
 * @param data2 passed to the function as second parameter
 * @param cpu core to run function on
 */
static void inline doRemoteOp(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t cpu)
{
    ipi_remote_call(func, data1, data2, data3, BIT(cpu));
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
    ipi_remote_call(func, 0, 0, 0, mask);
}

static void inline doRemoteMaskOp1Arg(IpiRemoteCall_t func, word_t data1, word_t mask)
{
    ipi_remote_call(func, data1, 0, 0, mask);
}

static void inline doRemoteMaskOp2Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t mask)
{
    ipi_remote_call(func, data1, data2, 0, mask);
}

static void inline doRemoteMaskOp3Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t data3, word_t mask)
{
    ipi_remote_call(func, data1, data2, data3, mask);
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

#ifdef CONFIG_HAVE_FPU
static inline void doRemoteswitchFpuOwner(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_switchFpuOwner, cpu);
}
#endif /* CONFIG_HAVE_FPU */

/* Returns false when the caller needs to retry the call later */
bool_t doRemoteStall(word_t cpu);

/* This is an asynchronous call and returns immediately.
 *
 * @param mask cores to request rescheduling
 */
void doMaskReschedule(word_t mask);


#ifdef CONFIG_DEBUG_BUILD
exception_t handle_SysDebugSendIPI(void);
#endif

#endif /* ENABLE_SMP_SUPPORT */
