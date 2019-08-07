/*
 * Copyright 2018, Data61
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
    IpiRemoteCall_switchFpuOwner,
    IpiNumArchRemoteCall
} IpiRemoteCall_t;

void ipi_send_target(irq_t irq, word_t cpuTargetList);
irq_t ipi_get_irq(void);
void  ipi_clear_irq(irq_t irq);

#else
static inline irq_t ipi_get_irq(void)
{
    return irqInvalid;
}

static inline void  ipi_clear_irq(irq_t irq)
{
    return;
}
#endif

#endif /* __ARCH_SMP_IPI_H */
