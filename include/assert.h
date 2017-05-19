/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ASSERT_H
#define __ASSERT_H

#include <config.h>
#include <util.h>

#ifdef CONFIG_DEBUG_BUILD

void _fail(
    const char*  str,
    const char*  file,
    unsigned int line,
    const char*  function
) NORETURN;

#define fail(s) _fail(s, __FILE__, __LINE__, __func__)

void _assert_fail(
    const char*  assertion,
    const char*  file,
    unsigned int line,
    const char*  function
) NORETURN;

#define assert(expr) \
    if(!(expr)) _assert_fail(#expr, __FILE__, __LINE__, __FUNCTION__)

#else /* !DEBUG */

#define fail(s) halt()

#define assert(expr)

#endif /* DEBUG */

/* Create an assert that will trigger a compile error if it fails. */
#define compile_assert(name, expr) \
        typedef int __assert_failed_##name[(expr) ? 1 : -1];

/* Sometimes compile asserts contain expressions that the C parser cannot
 * handle. For such expressions unverified_compile_assert should be used. */
#ifdef CONFIG_VERIFICATION_BUILD
#define unverified_compile_assert(name, expr)
#else
#define unverified_compile_assert compile_assert
#endif

#endif
