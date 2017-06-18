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

#ifndef __MODE_SMP_IPI_H
#define __MODE_SMP_IPI_H

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <smp/ipi.h>

#ifdef ENABLE_SMP_SUPPORT

typedef enum {
    IpiRemoteCall_InvalidateTLBEntry = IpiNumArchRemoteCall,
    IpiRemoteCall_InvalidatePageStructureCache,
    IpiRemoteCall_InvalidateTLB,
    IpiNumModeRemoteCall
} IpiModeRemoteCall_t;

static void inline doRemoteInvalidateTLBEntry(vptr_t vptr, word_t mask)
{
    doRemoteMaskOp1Arg(IpiRemoteCall_InvalidateTLBEntry, vptr, mask);
}

static void inline doRemoteInvalidatePageStructureCache(word_t mask)
{
    doRemoteMaskOp0Arg(IpiRemoteCall_InvalidatePageStructureCache, mask);
}

static void inline doRemoteInvalidateTLB(word_t mask)
{
    doRemoteMaskOp0Arg(IpiRemoteCall_InvalidateTLB, mask);
}

void Mode_handleRemoteCall(IpiModeRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2);
#endif /* ENABLE_SMP_SUPPORT */
#endif /* __ARCH_SMP_IPI_H */
