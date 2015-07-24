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

#include <util.h>
#include <arch/types.h>
#include <plat/machine/io.h>

#if defined DEBUG || defined RELEASE_PRINTF
unsigned int puts(const char *s) VISIBLE;
/* for prints that you want enabled in both DEBUG and RELEASE_PRINTF modes,
   use kprintf directly */
unsigned int kprintf(const char *format, ...) VISIBLE;
unsigned int print_unsigned_long(unsigned long x, unsigned int ui_base) VISIBLE;
#endif

#ifdef DEBUG
/* printf will result in output */
#define printf(args...) kprintf(args)
#else
/* printf will NOT result in output */
#define printf(args...) ((void)(0))
/* and neither will puts */
#define puts(s) ((void)(0))
#endif

#ifdef RELEASE_PRINTF
/* release_printfs will result in output */
#define release_printf(args...) kprintf(args)
#else
/* release_printfs will NOT result in output */
#define release_printf(args...) ((void)(0))
#endif

#endif
