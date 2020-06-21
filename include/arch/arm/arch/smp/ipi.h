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
    IpiRemoteCall_InvalidateTranslationSingle,
    IpiRemoteCall_InvalidateTranslationASID,
    IpiRemoteCall_InvalidateTranslationAll,
    IpiRemoteCall_switchFpuOwner,
    IpiRemoteCall_MaskPrivateInterrupt,
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    IpiRemoteCall_VCPUInjectInterrupt,
#endif
    /* Add relevant calls here upon required */
    IpiNumArchRemoteCall
} IpiRemoteCall_t;

#endif /* ENABLE_SMP_SUPPORT */

