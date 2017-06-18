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
    IpiRemoteCall_InvalidatePCID = IpiNumArchRemoteCall,
    IpiRemoteCall_InvalidateASID,
    IpiNumModeRemoteCall
} IpiModeRemoteCall_t;

void Mode_handleRemoteCall(IpiModeRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2);

static inline void doRemoteInvalidatePCID(word_t type, void *vaddr, asid_t asid, word_t mask)
{
    doRemoteMaskOp3Arg(IpiRemoteCall_InvalidatePCID, type, (word_t)vaddr, asid, mask);
}

static inline void doRemoteInvalidateASID(vspace_root_t *vspace, asid_t asid, word_t mask)
{
    doRemoteMaskOp2Arg(IpiRemoteCall_InvalidateASID, (word_t)vspace, asid, mask);
}

#endif /* ENABLE_SMP_SUPPORT */
#endif /* __ARCH_SMP_IPI_H */
