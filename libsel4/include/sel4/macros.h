/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef __LIBSEL4_MACROS_H
#define __LIBSEL4_MACROS_H

#include <autoconf.h>

/*
 * Some compilers attempt to pack enums into the smallest possible type.
 * For ABI compatibility with the kernel, we need to ensure they remain
 * the same size as a 'long'.
 */
#define SEL4_FORCE_LONG_ENUM(type) \
        _enum_pad_ ## type = (1ULL << ((sizeof(long)*8) - 1)) - 1

#ifndef CONST
#define CONST        __attribute__((__const__))
#endif

#ifndef PURE
#define PURE         __attribute__((__pure__))
#endif

#define SEL4_OFFSETOF(type, member) __builtin_offsetof(type, member)

#define SEL4_PACKED __attribute__((packed))

#ifdef CONFIG_LIB_SEL4_INLINE_INVOCATIONS
#define LIBSEL4_INLINE static inline
#define LIBSEL4_INLINE_FUNC static inline

#elif defined(CONFIG_LIB_SEL4_PUBLIC_SYMBOLS)
#define LIBSEL4_INLINE __attribute__((unused)) __attribute__((weak))
#define LIBSEL4_INLINE_FUNC __attribute__((unused)) __attribute__((weak))

#else
#define LIBSEL4_INLINE __attribute__((noinline)) __attribute__((unused)) __attribute__((weak))
#define LIBSEL4_INLINE_FUNC static inline
#endif

#define SEL4_DEPRECATED(x) __attribute__((deprecated(x)))
#define SEL4_DEPRECATE_MACRO(x) _Pragma("deprecated") x

#define SEL4_COMPILE_ASSERT(name, expr) typedef int __assert_failed_##name[(expr) ? 1 : -1];
#define SEL4_SIZE_SANITY(index, entry, size) SEL4_COMPILE_ASSERT(index##entry##size, index + entry == size)
#endif
