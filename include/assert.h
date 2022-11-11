/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>

#ifdef CONFIG_DEBUG_BUILD

void _fail(
    const char  *str,
    const char  *file,
    unsigned int line,
    const char  *function
) NORETURN;

#define fail(s) _fail(s, __FILE__, __LINE__, __func__)

void _assert_fail(
    const char  *assertion,
    const char  *file,
    unsigned int line,
    const char  *function
) NORETURN;

#define assert(expr) \
    do { \
        if (!(expr)) { \
            _assert_fail(#expr, __FILE__, __LINE__, __FUNCTION__); \
        } \
    } while(0)

#else /* !DEBUG */

#define fail(s) halt()

#define assert(expr)

#endif /* DEBUG */

/* Create an assert that triggers a compile error if the condition fails. We do
 * not include sel4/macros.h that provides SEL4_COMPILE_ASSERT() for two
 * reasons:
 * - The kernel's source internals shall not have any unnecessary dependency on
 *     the user interface headers.
 * - The kernel user API headers aims to be compiler agnostic and stick to the
 *     standard(s). As _Static_assert() is a c11 feature, the c99 used for
 *     kernel compilation would use a helper macro. While this works, it
 *     creates strange error messages when the condition fails. Since kernel
 *     compilation supports just gcc and clang, and both are known to provide
 *     _Static_assert() even in c99, we can just use this.
 *
 * Unfortunately, the C parser does not understand _Static_assert(), so there is
 * still the need for the helper macro there. In addition, the macro
 * unverified_compile_assert() exists, because some compile asserts contain
 * expressions that the C parser cannot handle, too.
 */
#ifdef CONFIG_VERIFICATION_BUILD
#define compile_assert(name, expr) \
        typedef int __assert_failed_##name[(expr) ? 1 : -1] UNUSED;
#define unverified_compile_assert(name, expr)
#else /* not CONFIG_VERIFICATION_BUILD */
#define compile_assert(name, expr) _Static_assert(expr, #name);
#define unverified_compile_assert(name, expr) compile_assert(name, expr)
#endif /* [not] CONFIG_VERIFICATION_BUILD */
