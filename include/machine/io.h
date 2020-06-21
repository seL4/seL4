/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <arch/types.h>

#define FORMAT(archetype, string_index, first_to_check) \
        __attribute__((format(archetype, string_index, first_to_check)))

#if (defined CONFIG_DEBUG_BUILD) || (defined CONFIG_PRINTING)
void putDebugChar(unsigned char c);
#endif

#ifdef CONFIG_DEBUG_BUILD
/* io for dumping capdl */
unsigned char getDebugChar(void);
#endif

#ifdef CONFIG_PRINTING
/* printf will result in output */
void putchar(char c);
#define kernel_putchar(c) putchar(c)
word_t kprintf(const char *format, ...) VISIBLE FORMAT(printf, 1, 2);
word_t ksnprintf(char *str, word_t size, const char *format, ...);
word_t puts(const char *s) VISIBLE;
#define printf(args...) kprintf(args)
#define snprintf(args...) ksnprintf(args)
#else /* CONFIG_PRINTING */
/* printf will NOT result in output */
#define kernel_putchar(c) ((void)(0))
#define printf(args...) ((void)(0))
#define puts(s) ((void)(0))
#endif /* CONFIG_PRINTING */
