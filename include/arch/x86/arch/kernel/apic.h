/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_APIC_H
#define __ARCH_KERNEL_APIC_H

#include <types.h>
#include <plat/machine.h>

paddr_t apic_get_base_paddr(void);
bool_t apic_init(bool_t mask_legacy_irqs);
cpu_id_t apic_get_id(void);

bool_t apic_is_interrupt_pending(void);
void apic_ack_active_interrupt(void);

void apic_send_init_ipi(cpu_id_t cpu_id);
void apic_send_startup_ipi(cpu_id_t cpu_id, paddr_t startup_addr);

#endif
