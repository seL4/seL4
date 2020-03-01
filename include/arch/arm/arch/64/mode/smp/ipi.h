/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
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