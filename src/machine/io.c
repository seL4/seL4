/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/io.h>

#ifdef CONFIG_PRINTING

#include <stdarg.h>

/*
 * a handle defining how to output a character
 */
typedef void (*out_fn)(char character, char *buf, word_t idx);

/*
 * structure to allow a generic vprintf
 * a out_fn handle and a buf to work on
 */
typedef struct {
    out_fn putchar;
    char *buf;
    word_t idx;
    word_t maxlen;
} out_wrap_t;

/*
 * putchar would then just call the handle with its buf
 * and current idx and then increment idx
 */
static void putchar_wrap(out_wrap_t *out, char c)
{
    if (out->maxlen < 0 || out->idx < out->maxlen) {
        out->putchar(c, out->buf, out->idx);
        out->idx++;
    }
}


void putchar(char c)
{
    if (c == '\n') {
        putDebugChar('\r');
    }
    putDebugChar(c);
}

static unsigned int print_spaces(out_wrap_t *out, int n)
{
    for (int i = 0; i < n; i++) {
        putchar_wrap(out, ' ');
    }

    return n;
}

static unsigned int print_string(out_wrap_t *out, const char *s)
{
    unsigned int n;

    for (n = 0; *s; s++, n++) {
        putchar_wrap(out, *s);
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

static word_t print_unsigned_long(out_wrap_t *out_wrap, unsigned long x, word_t ui_base)
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
        putchar_wrap(out_wrap, '0');
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
        putchar_wrap(out_wrap, out[j - 1]);
    }

    return i;
}

/* The print_unsigned_long_long function assumes that an unsinged int
   is half the size of an unsigned long long */
compile_assert(print_unsigned_long_long_sizes, sizeof(unsigned int) * 2 == sizeof(unsigned long long))

static unsigned int
print_unsigned_long_long(out_wrap_t *out, unsigned long long x, unsigned int ui_base)
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
        n += print_unsigned_long(out, upper, ui_base);
        /* print leading 0s */
        while (!(mask & lower)) {
            putchar_wrap(out, '0');
            n++;
            mask = mask >> 4;
            shifts++;
            if (shifts == 8) {
                break;
            }
        }
    }
    /* print last 32 bits */
    n += print_unsigned_long(out, lower, ui_base);

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

static int vprintf(out_wrap_t *out, const char *format, va_list ap)
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
                putchar_wrap(out, '%');
                n++;
                format++;
                break;

            case 'd': {
                int x = va_arg(ap, int);

                if (x < 0) {
                    putchar_wrap(out, '-');
                    n++;
                    x = -x;
                }

                n += print_unsigned_long(out, x, 10);
                format++;
                break;
            }

            case 'u':
                n += print_unsigned_long(out, va_arg(ap, unsigned int), 10);
                format++;
                break;

            case 'x':
                n += print_unsigned_long(out, va_arg(ap, unsigned int), 16);
                format++;
                break;

            case 'p': {
                unsigned long p = va_arg(ap, unsigned long);
                if (p == 0) {
                    n += print_string(out, "(nil)");
                } else {
                    n += print_string(out, "0x");
                    n += print_unsigned_long(out, p, 16);
                }
                format++;
                break;
            }

            case 's':
                n += print_string(out, va_arg(ap, char *));
                format++;
                break;

            case 'l':
                format++;
                switch (*format) {
                case 'd': {
                    long x = va_arg(ap, long);

                    if (x < 0) {
                        putchar_wrap(out, '-');
                        n++;
                        x = -x;
                    }

                    n += print_unsigned_long(out, (unsigned long)x, 10);
                    format++;
                }
                break;
                case 'l':
                    if (*(format + 1) == 'x') {
                        n += print_unsigned_long_long(out, va_arg(ap, unsigned long long), 16);
                    }
                    format += 2;
                    break;
                case 'u':
                    n += print_unsigned_long(out, va_arg(ap, unsigned long), 10);
                    format++;
                    break;
                case 'x':
                    n += print_unsigned_long(out, va_arg(ap, unsigned long), 16);
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

            if (nspaces > n) {
                n += print_spaces(out, nspaces - n);
            }
            nspaces = 0;
            formatting = 0;
        } else {
            switch (*format) {
            case '%':
                formatting = 1;
                format++;
                break;

            default:
                putchar_wrap(out, *format);
                n++;
                format++;
                break;
            }
        }
    }

    return n;
}

// sprintf fills its buf with the given character
static void buf_out_fn(char c, char *buf, word_t idx)
{
    buf[idx] = c;
}

// printf only needs to call kernel_putchar
static void kernel_out_fn(char c, char *buf, word_t idx)
{
    kernel_putchar(c);
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

    out_wrap_t out = { kernel_out_fn, NULL, 0, -1 };

    va_start(args, format);
    i = vprintf(&out, format, args);
    va_end(args);
    return i;
}

word_t ksnprintf(char *str, word_t size, const char *format, ...)
{
    va_list args;
    word_t i;

    out_wrap_t out = { buf_out_fn, str, 0, size };

    va_start(args, format);
    i = vprintf(&out, format, args);
    va_end(args);

    // make sure there is space for a 0 byte
    if (i >= size) {
        i = size - 1;
    }
    str[i] = 0;

    return i;
}

#endif /* CONFIG_PRINTING */
