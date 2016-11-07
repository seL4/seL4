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

#ifndef __MODE_KERNEL_IPI_H
#define __MODE_KERNEL_IPI_H

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <arch/kernel/ipi.h>

#if CONFIG_MAX_NUM_NODES > 1

typedef enum {
    IpiNumModeRemoteCall = IpiNumArchRemoteCall
} IpiModeRemoteCall_t;

static inline void
Mode_handleRemoteCall(IpiModeRemoteCall_t call, word_t arg0, word_t arg1)
{
    fail("Invalid IPI");
}

#endif /* CONFIG_MAX_NUM_NODES */
#endif /* __ARCH_KERNEL_IPI_H */
