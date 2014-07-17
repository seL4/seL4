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

#ifdef DEBUG
void omap3_uart_putchar(char c);
void putDebugChar(unsigned char c);
unsigned char getDebugChar(void);

#define kernel_putchar(c) omap3_uart_putchar(c)
#else /* !DEBUG */
#define kernel_putchar(c) ((void)(0))
#endif

#endif
