/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */
#pragma once

#include <config.h>
#include <arch/linker.h>
#include <types.h>

void out8(uint16_t port, uint8_t value);
void out16(uint16_t port, uint16_t value);
void out32(uint16_t port, uint32_t value);
uint8_t in8(uint16_t port);
uint16_t in16(uint16_t port);
uint32_t in32(uint16_t port);

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING)
void serial_init(uint16_t port);
#endif
