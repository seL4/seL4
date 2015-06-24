/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __LIBSEL4_IO_H
#define __LIBSEL4_IO_H

#ifdef DEBUG
void libsel4_putchar(const char c);
unsigned int libsel4_puts(const char *s);
unsigned int libsel4_printf(const char *format, ...);
unsigned int libsel4_print_unsigned_long(unsigned long x, unsigned int ui_base);
#else
#define libsel4_putchar(c) ((void)(0))
#define libsel4_puts(s) ((void)(0))
#define libsel4_printf(...) ((void)(0))
#define libsel4_print_unsigned_long(unsigned long x, unsigned int ui_base) ((void)(0))
#endif

#endif
