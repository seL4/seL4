/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_IOAPIC_H
#define __PLAT_MACHINE_IOAPIC_H

#ifdef CONFIG_IRQ_IOAPIC

#include <types.h>
#include <plat/machine.h>
#include <arch/types.h>

void ioapic_init(uint32_t num_nodes, cpu_id_t *cpu_list, uint32_t num_ioapic);
void ioapic_mask_irq(bool_t mask, irq_t irq);
void ioapic_set_mode(irq_t irq, bool_t levelTrigger, bool_t polarityLow);

#endif

#endif
