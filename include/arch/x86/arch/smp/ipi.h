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

#ifndef __ARCH_SMP_IPI_H
#define __ARCH_SMP_IPI_H

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
#endif /* __ARCH_SMP_IPI_H */
