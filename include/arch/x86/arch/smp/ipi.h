/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once
#include <config.h>

#ifdef ENABLE_SMP_SUPPORT
typedef enum {
    IpiRemoteCall_Stall,
#ifdef CONFIG_VTX
    IpiRemoteCall_ClearCurrentVCPU,
    IpiRemoteCall_VMCheckBoundNotification,
#endif
    IpiRemoteCall_InvalidatePageStructureCacheASID,
    IpiRemoteCall_InvalidateTranslationSingle,
    IpiRemoteCall_InvalidateTranslationSingleASID,
    IpiRemoteCall_InvalidateTranslationAll,
    IpiRemoteCall_switchFpuOwner,
    IpiNumArchRemoteCall
} IpiRemoteCall_t;

#endif /* ENABLE_SMP_SUPPORT */

