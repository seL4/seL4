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

#ifndef __ARCH_KERNEL_IPI_H
#define __ARCH_KERNEL_IPI_H

#include <config.h>
#include <types.h>
#include <plat/machine.h>

#if CONFIG_MAX_NUM_NODES > 1
#define MAX_IPI_ARGS    2   /* Maximum number of parameters to remote function */

void Arch_handleIPI(irq_t irq);

typedef enum {
    IpiRemoteCall_Stall,
#ifdef CONFIG_VTX
    IpiRemoteCall_ClearCurrentVCPU,
#endif
    IpiRemoteCall_InvalidatePageStructureCacheASID,
    IpiRemoteCall_InvalidateTranslationSingle,
    IpiRemoteCall_InvalidateTranslationSingleASID,
    IpiRemoteCall_InvalidateTranslationAll,
    IpiRemoteCall_switchFpuOwner,
    IpiNumArchRemoteCall
} IpiRemoteCall_t;

/*
 * Run a synchronous function on all cores specified by mask. Return when target cores
 * have all executed the function. Caller must hold the lock.
 *
 * @param func the function to run
 * @param data1 passed to the function as first parameter
 * @param data2 passed to the function as second parameter
 * @param mask cores to run function on
 */
void doRemoteMaskOp(IpiRemoteCall_t func, word_t data1, word_t data2, word_t mask);

/* Run a synchronous function on a core specified by cpu.
 *
 * @param func the function to run
 * @param data1 passed to the function as first parameter
 * @param data2 passed to the function as second parameter
 * @param cpu core to run function on
 */
static void inline doRemoteOp(IpiRemoteCall_t func, word_t data1, word_t data2, word_t cpu)
{
    doRemoteMaskOp(func, data1, data2, BIT(cpu));
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
    doRemoteMaskOp(func, 0, 0, mask);
}

static void inline
doRemoteMaskOp1Arg(IpiRemoteCall_t func, word_t data1, word_t mask)
{
    doRemoteMaskOp(func, data1, 0, mask);
}

static void inline
doRemoteMaskOp2Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t mask)
{
    doRemoteMaskOp(func, data1, data2, mask);
}

static void inline doRemoteOp0Arg(IpiRemoteCall_t func, word_t cpu)
{
    doRemoteOp(func, 0, 0, cpu);
}

static void inline
doRemoteOp1Arg(IpiRemoteCall_t func, word_t data1, word_t cpu)
{
    doRemoteOp(func, data1, 0, cpu);
}

static void inline
doRemoteOp2Arg(IpiRemoteCall_t func, word_t data1, word_t data2, word_t cpu)
{
    doRemoteOp(func, data1, data2, cpu);
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

static void inline doRemoteStall(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_Stall, cpu);
}

static void inline doRemoteswitchFpuOwner(user_fpu_state_t *new_owner, word_t cpu)
{
    doRemoteOp1Arg(IpiRemoteCall_switchFpuOwner, (word_t)new_owner, cpu);
}

static void inline doRemoteInvalidatePageStructureCacheASID(paddr_t root, asid_t asid, word_t mask)
{
    doRemoteMaskOp2Arg(IpiRemoteCall_InvalidatePageStructureCacheASID, root, asid, mask);
}

static void inline doRemoteInvalidateTranslationSingle(vptr_t vptr, word_t mask)
{
    doRemoteMaskOp1Arg(IpiRemoteCall_InvalidateTranslationSingle, vptr, mask);
}

static void inline doRemoteInvalidateTranslationSingleASID(vptr_t vptr, asid_t asid, word_t mask)
{
    doRemoteMaskOp2Arg(IpiRemoteCall_InvalidateTranslationSingleASID, vptr, asid, mask);
}

static void inline doRemoteInvalidateTranslationAll(word_t mask)
{
    doRemoteMaskOp0Arg(IpiRemoteCall_InvalidateTranslationAll, mask);
}

#ifdef CONFIG_VTX
static inline void
doRemoteClearCurrentVCPU(word_t cpu)
{
    doRemoteOp0Arg(IpiRemoteCall_ClearCurrentVCPU, cpu);
}
#endif

#endif /* CONFIG_MAX_NUM_NODES */
#endif /* __ARCH_KERNEL_IPI_H */
