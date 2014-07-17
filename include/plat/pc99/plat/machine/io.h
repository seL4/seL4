/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_IO_H
#define __PLAT_IO_H

#include <types.h>

void out8(uint16_t port, uint8_t value);
void out16(uint16_t port, uint16_t value);
void out32(uint16_t port, uint32_t value);
uint8_t in8(uint16_t port);
uint16_t in16(uint16_t port);
uint32_t in32(uint16_t port);

/* these versions are linked to physical addresses */
void out8_phys(uint16_t port, uint8_t value);
uint8_t in8_phys(uint16_t port);

#ifdef DEBUG

void serial_init(uint16_t port);
void console_putchar(char c);

#define kernel_putchar(c) console_putchar(c)

#else /* !DEBUG */

#define kernel_putchar(c) ((void)(0))

#endif

#endif
