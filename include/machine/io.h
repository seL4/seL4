/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_DEBUG_BUILD
/* io for dumping capdl */
unsigned char kernel_getDebugChar(void);
#endif


#ifdef CONFIG_PRINTING

#include <arch/types.h>
#include <stdarg.h>

void init_console(void);

/* the actual output function */
void kernel_putDebugChar(unsigned char c);

/* This is the actual implementation of the kernel printing API. It must never
 * be called directly from anywhere except the function defined in this file.
 */
int impl_kvprintf(const char *format, va_list ap);
int impl_ksnvprintf(char *str, word_t size, const char *format, va_list ap);

/*
 *------------------------------------------------------------------------------
 * Kernel printing API
 *------------------------------------------------------------------------------
 */

/* Writes a character to the kernel output channel. This is used to implement
 * the syscall SysDebugPutChar.
 */
static inline void kernel_putchar(
    char c)
{
    /* Write to target specific debug output channel. */
    kernel_putDebugChar(c);
}

/* Writes a character to the active output channel. This is used by all code
 * related to printf(). Contrary to the common signature of putchar(), there is
 * no return value here.
 */
static inline void putchar(
    char c)
{
    /* Write to target specific debug output channel. Purposely, we do not call
     * kernel_putchar() here, as the kernel printf() channel is semantically
     * different from the syscall SysDebugPutChar channel. The unification
     * of both channels happens at the lower layer eventually
     */
    kernel_putDebugChar(c);
}

/* Writes the string and a trailing newline. There is no point to enforce a
 * kernel_puts(), as this is just a wrapper for putchar() anyway.
 */
static inline int puts(
    const char *str)
{
    if (str) {
        while (*str) {
            putchar(*str++);
        }
    }
    putchar('\n');
    /* Standards define that a non-negative number is returned on success. */
    return 0;
}

/* There should only be a kprintf() that all kernel code must use for printing,
 * but for convenience we provide a printf() here.
 */
static inline __attribute__((format(printf, 1, 2))) int printf(
    const char *format,
    ...)
{
    va_list args;
    va_start(args, format);
    int ret = impl_kvprintf(format, args); /* will call putchar() eventually */
    va_end(args);
    return ret;
}

/* Provide the standard snprintf() for write formatted data into a buffer, which
 * can then be printed or stored.
 */
static inline __attribute__((format(printf, 3, 4))) int snprintf(
    char *buf,
    word_t size,
    const char *format,
    ...)
{
    va_list args;
    va_start(args, format);
    int ret = impl_ksnvprintf(buf, size, format, args);
    va_end(args);
    return ret;
}

#else /* not CONFIG_PRINTING */

/* The verification runs on the output of the preprocessing stage of a release
 * build configuration, CONFIG_PRINTING is not enabled there. We remove all
 * calls to printf() completely from the code base, because neither printf() nor
 * the usage of a variable argument list is supported by the verification
 * toolchain. It would just reject the code if it encounters any unsupported
 * things.
 */
#define printf(...)             ((void)(0))

/* Seems there is no need to define out these functions, they are use by code
 * that is active with CONFIG_PRINTING only.
 *
 *   #define kernel_putchar(...)     ((void)(0))
 *   #define putchar(...)            ((void)(0))
 *   #define puts(...)               ((void)(0))
 *   #define snprintf(...)           ((void)(0))
 */

#endif /* [not] CONFIG_PRINTING */
