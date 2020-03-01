/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __ARCH_SMP_IPI_H
#define __ARCH_SMP_IPI_H

#include <config.h>

#ifdef ENABLE_SMP_SUPPORT
typedef enum {
    IpiRemoteCall_Stall,
    IpiRemoteCall_switchFpuOwner,
    IpiNumArchRemoteCall
} IpiRemoteCall_t;

void ipi_send_target(irq_t irq, word_t cpuTargetList);
irq_t ipi_get_irq(void);
void  ipi_clear_irq(irq_t irq);
#endif

#endif /* __ARCH_SMP_IPI_H */
