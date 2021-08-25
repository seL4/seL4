/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * Portions derived from musl:
 *
 * Copyright © 2005-2020 Rich Felker, et al.
 *
 * SPDX-License-Identifier: MIT
 */

#include <config.h>
#include <machine/io.h>

#ifdef CONFIG_PRINTING

#include <stdarg.h>
#include <stdint.h>

/*
 *------------------------------------------------------------------------------
 * printf() core output channel management
 *------------------------------------------------------------------------------
 */

typedef struct _out_wrap_t  out_wrap_t;

/* handler defining how/where to actually output a buffer */
typedef void (*out_write_fn)(out_wrap_t *out, const char *buf, word_t len);

struct _out_wrap_t {
    const out_write_fn write;
    char *const buf;
    const word_t maxlen;
    word_t used;
};

/* printf_core() and its helpers call this to actually output something. The
 * parameter 'out_wrap' cam be NULL, e.g. when printf_core() is just caller to
 * validate the format string. In this case we do nothing.
 */
static void out(out_wrap_t *out_wrap, const char *buf, word_t len)
{
    if (out_wrap) {
        out_wrap->write(out_wrap, buf, len);
    }
}

/* An out_write_fn implementation to print the characters via putchar(). It is
 * guaranteed here that 'out' is not NULL. The current implementation also never
 * passes NULL for 'buf'. */
static void do_output_to_putchar(
    UNUSED out_wrap_t *out,
    const char *buf,
    word_t len)
{
    if (buf) {
        while (len-- > 0) {
            putchar(*buf++);
        }
    }
}

/* An out_write_fn implementation to copy the buffer into the out buffer. It is
 * guaranteed here that 'out' is not NULL. The current implementation also never
 * passes NULL for 'buf'. */
static void do_output_to_buffer(
    out_wrap_t *out,
    const char *buf,
    word_t len)
{
    /* It's guaranteed here that 'out' is not NULL. The current implementation
     * also never passes NULL for 'buf'. */
    if (buf && (out->used < out->maxlen)) {
        /* there is still space in the buffer*/
        word_t free = out->maxlen - out->used;
        if (len > free) {
            len = free;
        }
        memcpy(&out->buf[out->used], buf, len);
        out->used += len;
    }
}

/*
 *------------------------------------------------------------------------------
 * printf() core implementation
 *------------------------------------------------------------------------------
 */

static inline bool_t isdigit(char c)
{
    return c >= '0' &&
           c <= '9';
}

/* Convenient bit representation for modifier flags, which all fall
 * within 31 codepoints of the space character. */

#define MASK_TYPE(a) (1U<<( a -' '))

#define ALT_FORM     (1U<<('#'-' '))
#define ZERO_PAD     (1U<<('0'-' '))
#define LEFT_ADJ     (1U<<('-'-' '))
#define PAD_POS      (1U<<(' '-' '))
#define MARK_POS     (1U<<('+'-' '))
#define GROUPED      (1U<<('\''-' '))

#define FLAGMASK (ALT_FORM|ZERO_PAD|LEFT_ADJ|PAD_POS|MARK_POS|GROUPED)

#define INTMAX_MAX INT32_MAX
#define INT_MAX  0x7fffffff

#define ULONG_MAX ((unsigned long)(-1))

/* State machine to accept length modifiers + conversion specifiers.
 * Result is 0 on failure, or an argument type to pop on success. */

enum {
    BARE, LPRE, LLPRE, HPRE, HHPRE, BIGLPRE,
    ZTPRE, JPRE,
    STOP,
    PTR, INT, UINT, ULLONG,
    LONG, ULONG,
    SHORT, USHORT, CHAR, UCHAR,
    WORDT, LLONG,
#define IMAX LLONG
#define UMAX ULLONG
#define PDIFF LONG
#define UIPTR ULONG
    NOARG,
    MAXSTATE
};

#define S(x) [(x)-'A']

static const unsigned char states[]['z' - 'A' + 1] = {
    { /* 0: bare types */
        S('d') = INT, S('i') = INT,
        S('o') = UINT, S('u') = UINT, S('x') = UINT, S('X') = UINT,
        S('c') = CHAR,
        S('s') = PTR, S('p') = UIPTR, S('n') = PTR,
        S('l') = LPRE, S('h') = HPRE,
        S('z') = ZTPRE, S('j') = JPRE, S('t') = ZTPRE,
    }, { /* 1: l-prefixed */
        S('d') = LONG, S('i') = LONG,
        S('o') = ULONG, S('u') = ULONG, S('x') = ULONG, S('X') = ULONG,
        S('n') = PTR,
        S('l') = LLPRE,
    }, { /* 2: ll-prefixed */
        S('d') = LLONG, S('i') = LLONG,
        S('o') = ULLONG, S('u') = ULLONG,
        S('x') = ULLONG, S('X') = ULLONG,
        S('n') = PTR,
    }, { /* 3: h-prefixed */
        S('d') = SHORT, S('i') = SHORT,
        S('o') = USHORT, S('u') = USHORT,
        S('x') = USHORT, S('X') = USHORT,
        S('n') = PTR,
        S('h') = HHPRE,
    }, { /* 4: hh-prefixed */
        S('d') = CHAR, S('i') = CHAR,
        S('o') = UCHAR, S('u') = UCHAR,
        S('x') = UCHAR, S('X') = UCHAR,
        S('n') = PTR,
    }, { /* 5: L-prefixed not supported */
    }, { /* 6: z- or t-prefixed (assumed to be same size) */
        S('d') = PDIFF, S('i') = PDIFF,
        S('o') = WORDT, S('u') = WORDT,
        S('x') = WORDT, S('X') = WORDT,
        S('n') = PTR,
    }, { /* 7: j-prefixed */
        S('d') = IMAX, S('i') = IMAX,
        S('o') = UMAX, S('u') = UMAX,
        S('x') = UMAX, S('X') = UMAX,
        S('n') = PTR,
    }
};

#define OOB(x) ((unsigned)(x)-'A' > 'z'-'A')
#define DIGIT(c) (c - '0')

union arg {
    word_t i;
    long double f;
    void *p;
};

static void pop_arg(union arg *arg, int type, va_list *ap)
{
    switch (type) {
    case PTR:
        arg->p = va_arg(*ap, void *);
        break;
    case INT:
        arg->i = va_arg(*ap, int);
        break;
    case UINT:
        arg->i = va_arg(*ap, unsigned int);
        break;
    case LONG:
        arg->i = va_arg(*ap, long);
        break;
    case ULONG:
        arg->i = va_arg(*ap, unsigned long);
        break;
    case LLONG:
        arg->i = va_arg(*ap, long long);
        break;
    case ULLONG:
        arg->i = va_arg(*ap, unsigned long long);
        break;
    case SHORT:
        arg->i = (short)va_arg(*ap, int);
        break;
    case USHORT:
        arg->i = (unsigned short)va_arg(*ap, int);
        break;
    case CHAR:
        arg->i = (signed char)va_arg(*ap, int);
        break;
    case UCHAR:
        arg->i = (unsigned char)va_arg(*ap, int);
        break;
    case WORDT:
        arg->i = va_arg(*ap, word_t);
    }
}


static void pad(out_wrap_t *f, char c, int w, int l, int fl)
{
    char pad[32]; /* good enough for what the kernel prints */
    if (fl & (LEFT_ADJ | ZERO_PAD) || l >= w) {
        return;
    }
    l = w - l;
    memset(pad, c, l > sizeof(pad) ? sizeof(pad) : l);
    for (; l >= sizeof(pad); l -= sizeof(pad)) {
        out(f, pad, sizeof(pad));
    }
    out(f, pad, l);
}

static const char xdigits[16] = {
    "0123456789ABCDEF"
};

static char *fmt_x(word_t x, char *s, int lower)
{
    for (; x; x >>= 4) {
        *--s = xdigits[(x & 15)] | lower;
    }
    return s;
}

static char *fmt_o(word_t x, char *s)
{
    for (; x; x >>= 3) {
        *--s = '0' + (x & 7);
    }
    return s;
}

static char *fmt_u(word_t x, char *s)
{
    unsigned long y;
    for (; x > ULONG_MAX; x /= 10) {
        *--s = '0' + x % 10;
    }
    for (y = x;           y; y /= 10) {
        *--s = '0' + y % 10;
    }
    return s;
}

// Maximum buffer size taken to ensure correct adaptation
// However, it could be reduced/removed if we could measure
// the buf length under all code paths
#define LDBL_MANT_DIG 113

#define NL_ARGMAX 9

static int getint(char **s)
{
    int i;
    for (i = 0; isdigit(**s); (*s)++) {
        if (i > INT_MAX / 10U || DIGIT(**s) > INT_MAX - 10 * i) {
            i = -1;
        } else {
            i = 10 * i + DIGIT(**s);
        }
    }
    return i;
}

static int printf_core(out_wrap_t *f, const char *fmt, va_list *ap, union arg *nl_arg, int *nl_type)
{
    char *a, *z, *s = (char *)fmt;
    unsigned l10n = 0, fl;
    int w, p, xp;
    union arg arg;
    int argpos;
    unsigned st, ps;
    int cnt = 0, l = 0;
    word_t i;
    char buf[sizeof(word_t) * 3 + 3 + LDBL_MANT_DIG / 4];
    const char *prefix;
    int t, pl;

    for (;;) {
        /* This error is only specified for snprintf, but since it's
         * unspecified for other forms, do the same. Stop immediately
         * on overflow; otherwise %n could produce wrong results. */
        if (l > INT_MAX - cnt) {
            goto overflow;
        }

        /* Update output count, end loop when fmt is exhausted */
        cnt += l;
        if (!*s) {
            break;
        }

        /* Handle literal text and %% format specifiers */
        for (a = s; *s && *s != '%'; s++);
        for (z = s; s[0] == '%' && s[1] == '%'; z++, s += 2);
        if (z - a > INT_MAX - cnt) {
            goto overflow;
        }
        l = z - a;
        out(f, a, l);
        if (l) {
            continue;
        }

        if (isdigit(s[1]) && s[2] == '$') {
            l10n = 1;
            argpos = DIGIT(s[1]);
            s += 3;
        } else {
            argpos = -1;
            s++;
        }

        /* Read modifier flags */
        for (fl = 0; (unsigned)*s - ' ' < 32 && (FLAGMASK & MASK_TYPE(*s)); s++) {
            fl |= MASK_TYPE(*s);
        }

        /* Read field width */
        if (*s == '*') {
            if (isdigit(s[1]) && s[2] == '$') {
                l10n = 1;
                nl_type[DIGIT(s[1])] = INT;
                w = nl_arg[DIGIT(s[1])].i;
                s += 3;
            } else if (!l10n) {
                w = f ? va_arg(*ap, int) : 0;
                s++;
            } else {
                goto inval;
            }
            if (w < 0) {
                fl |= LEFT_ADJ;
                w = -w;
            }
        } else if ((w = getint(&s)) < 0) {
            goto overflow;
        }

        /* Read precision */
        if (*s == '.' && s[1] == '*') {
            if (isdigit(s[2]) && s[3] == '$') {
                nl_type[DIGIT(s[2])] = INT;
                p = nl_arg[DIGIT(s[2])].i;
                s += 4;
            } else if (!l10n) {
                p = f ? va_arg(*ap, int) : 0;
                s += 2;
            } else {
                goto inval;
            }
            xp = (p >= 0);
        } else if (*s == '.') {
            s++;
            p = getint(&s);
            xp = 1;
        } else {
            p = -1;
            xp = 0;
        }

        /* Format specifier state machine */
        st = 0;
        do {
            if (OOB(*s)) {
                goto inval;
            }
            ps = st;
            st = states[st]S(*s++);
        } while (st - 1 < STOP);
        if (!st) {
            goto inval;
        }

        /* Check validity of argument type (nl/normal) */
        if (st == NOARG) {
            if (argpos >= 0) {
                goto inval;
            }
        } else {
            if (argpos >= 0) {
                nl_type[argpos] = st;
                arg = nl_arg[argpos];
            } else if (f) {
                pop_arg(&arg, st, ap);
            } else {
                return 0;
            }
        }

        if (!f) {
            continue;
        }

        z = buf + sizeof(buf);
        prefix = "-+   0X0x";
        pl = 0;
        t = s[-1];

        /* - and 0 flags are mutually exclusive */
        if (fl & LEFT_ADJ) {
            fl &= ~ZERO_PAD;
        }

        if (t == 'n') {
            if (!arg.p) {
                continue;
            }
            switch (ps) {
            case BARE:
                *(int *)arg.p = cnt;
                break;
            case LPRE:
                *(long *)arg.p = cnt;
                break;
            case LLPRE:
                *(long long *)arg.p = cnt;
                break;
            case HPRE:
                *(unsigned short *)arg.p = cnt;
                break;
            case HHPRE:
                *(unsigned char *)arg.p = cnt;
                break;
            case ZTPRE:
                *(word_t *)arg.p = cnt;
                break;
            case JPRE:
                *(word_t *)arg.p = cnt;
                break;
            }
            continue;
        } else if (t == 'c') {
            p = 1;
            a = z - p;
            *a = arg.i;
            fl &= ~ZERO_PAD;
        } else if (t == 's') {
            a = arg.p ? arg.p : "(null)";
            z = a + strnlen(a, p < 0 ? INT_MAX : p);
            if (p < 0 && *z) {
                goto overflow;
            }
            p = z - a;
            fl &= ~ZERO_PAD;
        } else {
            switch (t) {
            case 'p':
                p = MAX(p, 2 * sizeof(void *));
                t = 'x';
                fl |= ALT_FORM;
            case 'x':
            case 'X':
                a = fmt_x(arg.i, z, t & 32);
                if (arg.i && (fl & ALT_FORM)) {
                    prefix += (t >> 4);
                    pl = 2;
                }
                break;
            case 'o':
                a = fmt_o(arg.i, z);
                if ((fl & ALT_FORM) && p < (z - a + 1)) {
                    p = z - a + 1;
                }
                break;
            case 'd':
            case 'i':
                pl = 1;
                if (arg.i > INTMAX_MAX) {
                    arg.i = -arg.i;
                } else if (fl & MARK_POS) {
                    prefix++;
                } else if (fl & PAD_POS) {
                    prefix += 2;
                } else {
                    pl = 0;
                }
            case 'u':
                a = fmt_u(arg.i, z);
                break;
            }
            if (xp && p < 0) {
                goto overflow;
            }
            if (xp) {
                fl &= ~ZERO_PAD;
            }
            if (!arg.i && !p) {
                a = z;
            } else {
                p = MAX(p, z - a + !arg.i);
            }
        }

        if (p < z - a) {
            p = z - a;
        }
        if (p > INT_MAX - pl) {
            goto overflow;
        }
        if (w < pl + p) {
            w = pl + p;
        }
        if (w > INT_MAX - cnt) {
            goto overflow;
        }

        pad(f, ' ', w, pl + p, fl);
        out(f, prefix, pl);
        pad(f, '0', w, pl + p, fl ^ ZERO_PAD);
        pad(f, '0', p, z - a, 0);
        out(f, a, z - a);
        pad(f, ' ', w, pl + p, fl ^ LEFT_ADJ);

        l = w;
    }

    if (f) {
        return cnt;
    }
    if (!l10n) {
        return 0;
    }

    for (i = 1; i <= NL_ARGMAX && nl_type[i]; i++) {
        pop_arg(nl_arg + i, nl_type[i], ap);
    }
    for (; i <= NL_ARGMAX && !nl_type[i]; i++);
    if (i <= NL_ARGMAX) {
        goto inval;
    }
    return 1;

// goto for potential debug error support
inval:
overflow:
    return -1;
}

static int vprintf(out_wrap_t *out, const char *fmt, va_list ap)
{
    va_list ap2;
    int nl_type[NL_ARGMAX + 1] = {0};
    union arg nl_arg[NL_ARGMAX + 1];
    int ret;

    // validate format string
    va_copy(ap2, ap);
    if (printf_core(NULL, fmt, &ap2, nl_arg, nl_type) < 0) {
        va_end(ap2);
        return -1;
    }

    ret = printf_core(out, fmt, &ap2, nl_arg, nl_type);
    va_end(ap2);
    return ret;
}

/*
 *------------------------------------------------------------------------------
 * Kernel printing API
 *------------------------------------------------------------------------------
 */

int impl_kvprintf(const char *format, va_list ap)
{
    out_wrap_t out_wrap =  {
        .write  = do_output_to_putchar,
        .buf    = NULL,
        .maxlen = 0,
        .used   = 0
    };

    return vprintf(&out_wrap, format, ap);
}

int impl_ksnvprintf(char *str, word_t size, const char *format, va_list ap)
{
    if (!str) {
        size = 0;
    }

    out_wrap_t out_wrap =  {
        .write  = do_output_to_buffer,
        .buf    = str,
        .maxlen = size,
        .used   = 0
    };

    int ret = vprintf(&out_wrap, format, ap);

    /* We return the number of characters written into the buffer, excluding the
     * terminating null char. However, we do never write more than 'size' bytes,
     * that includes the terminating null char. If the output was truncated due
     * to this limit, then the return value is the number of chars excluding the
     * terminating null byte, which would have been written to the buffer, if
     * enough space had been available. Thus, a return value of 'size' or more
     * means that the output was truncated.
     */
    if ((ret > 0) && (size > 0)) {
        str[(ret < size) ? ret : size - 1] = '\0';
    }

    return ret;
}

#endif /* CONFIG_PRINTING */
