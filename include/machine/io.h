/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MACHINE_IO_H_
#define __MACHINE_IO_H_

#include <config.h>
#include <util.h>
#include <arch/types.h>

#define FORMAT(archetype, string_index, first_to_check) \
        __attribute__((format(archetype, string_index, first_to_check)))

#if (defined CONFIG_DEBUG_BUILD) || (defined CONFIG_PRINTING)
/* most arm platforms just call this for putConsoleChar, so
 * prototype it here */
void putDebugChar(unsigned char c);
#endif

#ifdef CONFIG_DEBUG_BUILD
/* io for dumping capdl */
unsigned char getDebugChar(void);
#endif

#ifdef CONFIG_PRINTING
/* printf will result in output */
void putConsoleChar(unsigned char c);
void putchar(char c);
#define kernel_putchar(c) putchar(c)
word_t kprintf(const char *format, ...) VISIBLE FORMAT(printf, 1, 2);
word_t puts(const char *s) VISIBLE;
word_t print_unsigned_long(unsigned long x, word_t ui_base) VISIBLE;
#define printf(args...) kprintf(args)
#else /* CONFIG_PRINTING */
/* printf will NOT result in output */
#define kernel_putchar(c) ((void)(0))
#define printf(args...) ((void)(0))
#define puts(s) ((void)(0))
#endif /* CONFIG_PRINTING */

#endif /* __MACHINE_IO_H_ */
