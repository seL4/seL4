/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once
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
    doRemoteMaskOp1Arg((IpiRemoteCall_t)IpiRemoteCall_InvalidateTLBEntry, vptr, mask);
}

static void inline doRemoteInvalidatePageStructureCache(word_t mask)
{
    doRemoteMaskOp0Arg((IpiRemoteCall_t)IpiRemoteCall_InvalidatePageStructureCache, mask);
}

static void inline doRemoteInvalidateTLB(word_t mask)
{
    doRemoteMaskOp0Arg((IpiRemoteCall_t)IpiRemoteCall_InvalidateTLB, mask);
}

void Mode_handleRemoteCall(IpiModeRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2);
#endif /* ENABLE_SMP_SUPPORT */

