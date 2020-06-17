/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <arch/kernel/xapic.h>
#include <arch/kernel/x2apic.h>

BOOT_CODE bool_t apic_enable(void);
BOOT_CODE void apic_send_init_ipi(cpu_id_t cpu_id);
BOOT_CODE void apic_send_startup_ipi(cpu_id_t cpu_id, paddr_t startup_addr);
BOOT_CODE paddr_t apic_get_base_paddr(void);
BOOT_CODE bool_t apic_init(bool_t mask_legacy_irqs);

uint32_t apic_read_reg(apic_reg_t reg);
void apic_write_reg(apic_reg_t reg, uint32_t val);
void apic_write_icr(word_t high, word_t low);

logical_id_t apic_get_logical_id(void);
word_t apic_get_cluster(logical_id_t logical_id);
void apic_ack_active_interrupt(void);
bool_t apic_is_interrupt_pending(void);

void apic_send_ipi_core(irq_t vector, cpu_id_t cpu_id);
void apic_send_ipi_cluster(irq_t vector, word_t mda);

#define ipi_send_target apic_send_ipi_core

