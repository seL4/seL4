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

#include <util.h>

#ifdef DEBUG

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

/* Create an assert that will trigger a compile error if it fails. */
#define compile_assert(name, expr) \
        typedef int __assert_failed_##name[(expr) ? 1 : -1];

#else /* !DEBUG */

#define fail(s) halt()

#define assert(expr)

#define compile_assert(name, expr)

#endif /* DEBUG */

#endif
