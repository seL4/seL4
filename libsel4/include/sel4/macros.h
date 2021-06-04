/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>

#ifndef CONST
#define CONST   __attribute__((__const__))
#endif

#ifndef PURE
#define PURE    __attribute__((__pure__))
#endif

#define SEL4_PACKED             __attribute__((packed))
#define SEL4_DEPRECATED(x)      __attribute__((deprecated(x)))
#define SEL4_DEPRECATE_MACRO(x) _Pragma("deprecated") x
#define SEL4_OFFSETOF(type, member) __builtin_offsetof(type, member)

#define LIBSEL4_UNUSED          __attribute__((unused))
#define LIBSEL4_WEAK            __attribute__((weak))
#define LIBSEL4_NOINLINE        __attribute__((noinline))


#ifdef CONFIG_LIB_SEL4_INLINE_INVOCATIONS

#define LIBSEL4_INLINE          static inline
#define LIBSEL4_INLINE_FUNC     static inline

#elif defined(CONFIG_LIB_SEL4_PUBLIC_SYMBOLS)

#define LIBSEL4_INLINE          LIBSEL4_UNUSED LIBSEL4_WEAK
#define LIBSEL4_INLINE_FUNC     LIBSEL4_UNUSED LIBSEL4_WEAK

#else

#define LIBSEL4_INLINE          LIBSEL4_NOINLINE LIBSEL4_UNUSED LIBSEL4_WEAK
#define LIBSEL4_INLINE_FUNC     static inline

#endif

/* _Static_assert() is a c11 feature. Since the kernel is currently compiled
 * with c99, we have to emulate it. */
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define SEL4_COMPILE_ASSERT(name, expr)   _Static_assert(expr, #name);
#else
#define SEL4_COMPILE_ASSERT(name, expr) \
    typedef int __assert_failed_##name[(expr) ? 1 : -1] LIBSEL4_UNUSED;
#endif


#define SEL4_SIZE_SANITY(index, entry, size) \
    SEL4_COMPILE_ASSERT(index##_##entry##_##size, (index) + (entry) == size)


/*
 * Some compilers attempt to pack enums into the smallest possible type.
 * For ABI compatibility with the kernel, we need to ensure they remain
 * the same size as a 'long'.
 */
#define SEL4_FORCE_LONG_ENUM(type) \
    _enum_pad_ ## type = ((1ULL << ((sizeof(long)*8) - 1)) - 1)

#define LIBSEL4_BIT(n)  (1ul<<(n))
