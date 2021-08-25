/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <mode/smp/ipi.h>

#ifdef ENABLE_SMP_SUPPORT

void Mode_handleRemoteCall(IpiModeRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2)
{
    switch (call) {
    case IpiRemoteCall_InvalidateTLBEntry:
        invalidateLocalTLBEntry(arg0);
        break;

    case IpiRemoteCall_InvalidatePageStructureCache:
        invalidateLocalPageStructureCache();
        break;

    case IpiRemoteCall_InvalidateTLB:
        invalidateLocalTLB();
        break;

    default:
        fail("Invalid remote call");
    }
}

#endif /* ENABLE_SMP_SUPPORT */
