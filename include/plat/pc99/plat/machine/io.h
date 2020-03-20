/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

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

