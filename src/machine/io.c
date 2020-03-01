/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/io.h>

#ifdef CONFIG_PRINTING

#include <stdarg.h>

void putchar(char c)
{
    if (c == '\n') {
        putDebugChar('\r');
    }
    putDebugChar(c);
}

static unsigned int print_spaces(int n)
{
    for (int i = 0; i < n; i++) {
        kernel_putchar(' ');
    }

    return n;
}

static unsigned int print_string(const char *s)
{
    unsigned int n;

    for (n = 0; *s; s++, n++) {
        kernel_putchar(*s);
    }

    return n;
}

static unsigned long xdiv(unsigned long x, unsigned int denom)
{
    switch (denom) {
    case 16:
        return x / 16;
    case 10:
        return x / 10;
    default:
        return 0;
    }
}

static unsigned long xmod(unsigned long x, unsigned int denom)
{
    switch (denom) {
    case 16:
        return x % 16;
    case 10:
        return x % 10;
    default:
        return 0;
    }
}

word_t print_unsigned_long(unsigned long x, word_t ui_base)
{
    char out[sizeof(unsigned long) * 2 + 3];
    word_t i, j;
    unsigned int d;

    /*
     * Only base 10 and 16 supported for now. We want to avoid invoking the
     * compiler's support libraries through doing arbitrary divisions.
     */
    if (ui_base != 10 && ui_base != 16) {
        return 0;
    }

    if (x == 0) {
        kernel_putchar('0');
        return 1;
    }

    for (i = 0; x; x = xdiv(x, ui_base), i++) {
        d = xmod(x, ui_base);

        if (d >= 10) {
            out[i] = 'a' + d - 10;
        } else {
            out[i] = '0' + d;
        }
    }

    for (j = i; j > 0; j--) {
        kernel_putchar(out[j - 1]);
    }

    return i;
}

/* The print_unsigned_long_long function assumes that an unsinged int
   is half the size of an unsigned long long */
compile_assert(print_unsigned_long_long_sizes, sizeof(unsigned int) * 2 == sizeof(unsigned long long))

static unsigned int
print_unsigned_long_long(unsigned long long x, unsigned int ui_base)
{
    unsigned int upper, lower;
    unsigned int n = 0;
    unsigned int mask = 0xF0000000u;
    unsigned int shifts = 0;

    /* only implemented for hex, decimal is harder without 64 bit division */
    if (ui_base != 16) {
        return 0;
    }

    /* we can't do 64 bit division so break it up into two hex numbers */
    upper = (unsigned int)(x >> 32llu);
    lower = (unsigned int) x & 0xffffffff;

    /* print first 32 bits if they exist */
    if (upper > 0) {
        n += print_unsigned_long(upper, ui_base);
        /* print leading 0s */
        while (!(mask & lower)) {
            kernel_putchar('0');
            n++;
            mask = mask >> 4;
            shifts++;
            if (shifts == 8) {
                break;
            }
        }
    }
    /* print last 32 bits */
    n += print_unsigned_long(lower, ui_base);

    return n;
}

static inline bool_t isdigit(char c)
{
    return c >= '0' &&
           c <= '9';
}

static inline int atoi(char c)
{
    return c - '0';
}

static int vprintf(const char *format, va_list ap)
{
    unsigned int n;
    unsigned int formatting;
    int nspaces = 0;

    if (!format) {
        return 0;
    }

    n = 0;
    formatting = 0;
    while (*format) {
        if (formatting) {
            while (isdigit(*format)) {
                nspaces = nspaces * 10 + atoi(*format);
                format++;
                if (format == NULL) {
                    break;
                }
            }
            switch (*format) {
            case '%':
                kernel_putchar('%');
                n++;
                format++;
                break;

            case 'd': {
                int x = va_arg(ap, int);

                if (x < 0) {
                    kernel_putchar('-');
                    n++;
                    x = -x;
                }

                n += print_unsigned_long(x, 10);
                format++;
                break;
            }

            case 'u':
                n += print_unsigned_long(va_arg(ap, unsigned int), 10);
                format++;
                break;

            case 'x':
                n += print_unsigned_long(va_arg(ap, unsigned int), 16);
                format++;
                break;

            case 'p': {
                unsigned long p = va_arg(ap, unsigned long);
                if (p == 0) {
                    n += print_string("(nil)");
                } else {
                    n += print_string("0x");
                    n += print_unsigned_long(p, 16);
                }
                format++;
                break;
            }

            case 's':
                n += print_string(va_arg(ap, char *));
                format++;
                break;

            case 'l':
                format++;
                switch (*format) {
                case 'd': {
                    long x = va_arg(ap, long);

                    if (x < 0) {
                        kernel_putchar('-');
                        n++;
                        x = -x;
                    }

                    n += print_unsigned_long((unsigned long)x, 10);
                    format++;
                }
                break;
                case 'l':
                    if (*(format + 1) == 'x') {
                        n += print_unsigned_long_long(va_arg(ap, unsigned long long), 16);
                    }
                    format += 2;
                    break;
                case 'u':
                    n += print_unsigned_long(va_arg(ap, unsigned long), 10);
                    format++;
                    break;
                case 'x':
                    n += print_unsigned_long(va_arg(ap, unsigned long), 16);
                    format++;
                    break;

                default:
                    /* format not supported */
                    return -1;
                }
                break;
            default:
                /* format not supported */
                return -1;
            }

            n += print_spaces(nspaces - n);
            nspaces = 0;
            formatting = 0;
        } else {
            switch (*format) {
            case '%':
                formatting = 1;
                format++;
                break;

            default:
                kernel_putchar(*format);
                n++;
                format++;
                break;
            }
        }
    }

    return n;
}

word_t puts(const char *s)
{
    for (; *s; s++) {
        kernel_putchar(*s);
    }
    kernel_putchar('\n');
    return 0;
}

word_t kprintf(const char *format, ...)
{
    va_list args;
    word_t i;

    va_start(args, format);
    i = vprintf(format, args);
    va_end(args);
    return i;
}

#endif /* CONFIG_PRINTING */
