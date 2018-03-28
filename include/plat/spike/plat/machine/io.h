/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __PLAT_IO_H
#define __PLAT_IO_H

#include <types.h>
#include <stdarg.h>

// RVTODO: the DEBUG macro is *not* meant to be checked in the kernel as there are too many
// notions of debug
#ifdef DEBUG
void putDebugChar(unsigned char c);
unsigned char getDebugChar(void);

#define verbose 5
#define _dprintf(v, col, args...) \
            do { \
                if ((v) < verbose){ \
                    printf(col); \
                    plogf(args); \
                    printf("\033[0;0m"); \
                } \
            } while (0)

#define dprintf(v, ...) _dprintf(v, "\033[22;33m", __VA_ARGS__)

#define WARN(...) _dprintf(-1, "\033[1;31mWARNING: ", __VA_ARGS__)

#define NOT_IMPLEMENTED() printf("\033[22;34m %s:%d -> %s not implemented\n\033[;0m",\
                                  __FILE__, __LINE__, __func__);

void plogf(const char *msg, ...)
{
    va_list alist;

    va_start(alist, msg);
    vprintf(msg, alist);
    va_end(alist);
}

#define kernel_putchar(c) putDebugChar(c)
#else /* !DEBUG */
#define kernel_putchar(c) ((void)(0))
#endif

#endif
