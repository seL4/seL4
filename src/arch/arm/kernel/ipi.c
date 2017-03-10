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

#include <config.h>
#include <arch/smp/ipi.h>
#include <smp/lock.h>

#if CONFIG_MAX_NUM_NODES > 1

void Arch_handleIPI(irq_t irq, bool_t irqPath)
{
#warning "Arch_handleIPI is not implemented"
}

void doMaskReschedule(word_t mask)
{
#warning "doMaskReschedule is not implemented"
}
#endif /* CONFIG_MAX_NUM_NODES */
