/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_IO_H
#define __PLAT_IO_H

#include <config.h>
#include <types.h>

#ifdef CONFIG_PRINTING
void tk1_uart_putchar(char c);

#define kernel_putchar(c) tk1_uart_putchar(c)
#else /* !CONFIG_PRINTING */
#define kernel_putchar(c) ((void)(0))
#endif

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING)
void putDebugChar(unsigned char c);
#endif

#ifdef CONFIG_DEBUG_BUILD
unsigned char getDebugChar(void);
#endif

#endif
