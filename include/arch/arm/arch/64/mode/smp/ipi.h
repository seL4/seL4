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
#include <arch/smp/ipi.h>
#include <smp/lock.h>

#ifdef ENABLE_SMP_SUPPORT

typedef enum {
    /* placeholder for 64-bit ARM IPI types */
    IpiNumModeRemoteCall
} IpiModeRemoteCall_t;

#endif /* ENABLE_SMP_SUPPORT */
#endif /* __MODE_SMP_IPI_H */