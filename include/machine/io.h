/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MACHINE_IO_H
#define __MACHINE_IO_H

#include <config.h>
#include <util.h>
#include <arch/types.h>
#include <plat/machine/io.h>

#define FORMAT(archetype, string_index, first_to_check) \
        __attribute__((format(archetype, string_index, first_to_check)))

#ifdef CONFIG_PRINTING
/* printf will result in output */
word_t kprintf(const char *format, ...) VISIBLE FORMAT(printf, 1, 2);
word_t puts(const char *s) VISIBLE;
word_t print_unsigned_long(unsigned long x, word_t ui_base) VISIBLE;
#define printf(args...) kprintf(args)
#else /* CONFIG_PRINTING */
/* printf will NOT result in output */
#define printf(args...) ((void)(0))
/* and neither will puts */
#define puts(s) ((void)(0))
#endif /* CONFIG_PRINTING */

#endif
