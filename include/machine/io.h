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

#ifdef DEBUG
unsigned int puts(const char *s) VISIBLE;
unsigned int printf(const char *format, ...) VISIBLE;
unsigned int print_unsigned_long(unsigned long x, unsigned int ui_base) VISIBLE;
#else
#define puts(s) ((void)(0))
#define printf(...) ((void)(0))
#endif

#endif
