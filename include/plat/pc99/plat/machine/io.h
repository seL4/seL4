/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_IO_H_
#define __PLAT_MACHINE_IO_H_

#include <config.h>
#include <linker.h>
#include <types.h>

/** MODIFIES: phantom_machine_state */
void out8(uint16_t port, uint8_t value);
/** MODIFIES: phantom_machine_state */
void out16(uint16_t port, uint16_t value);
/** MODIFIES: phantom_machine_state */
void out32(uint16_t port, uint32_t value);
/** MODIFIES: */
uint8_t in8(uint16_t port);
/** MODIFIES: */
uint16_t in16(uint16_t port);
/** MODIFIES: */
uint32_t in32(uint16_t port);

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING)
void serial_init(uint16_t port);
#endif

#endif /* __PLAT_MACHINE_IO_H_ */
