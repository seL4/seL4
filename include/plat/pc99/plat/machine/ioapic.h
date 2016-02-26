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

#include <types.h>
#include <plat/machine.h>
#include <arch/types.h>

void ioapic_init(uint32_t num_nodes, cpu_id_t *cpu_list, uint32_t num_ioapic);
void ioapic_mask(bool_t mask, uint32_t ioapic, uint32_t pin);

/* Checks if a pin can be mapped to an interrupt vector, returning
 * a suitable exception */
exception_t ioapic_decode_map_pin_to_vector(word_t ioapic, word_t pin, word_t level, word_t polarity, word_t vector);
/* The function maps a pin to an interrupt vector, does not perform
 * any checks, use ioapic_decode_map_pin_to_vector first */
void ioapic_map_pin_to_vector(word_t ioapic, word_t pin, word_t level, word_t polarity, word_t vector);
#endif
