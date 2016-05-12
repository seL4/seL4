/*
 * Copyright 2015, DornerWorks, Ltd.
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
void allwinnerA20_uart_putchar(char c);
void putDebugChar(unsigned char c);
unsigned char getDebugChar(void);

#define kernel_putchar(c) allwinnerA20_uart_putchar(c)
#else /* !CONFIG_PRINTING */
#define kernel_putchar(c) ((void)(0))
#endif

#endif
