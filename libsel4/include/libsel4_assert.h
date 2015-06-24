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


#ifdef DEBUG

void _fail(
    const char*  str,
    const char*  file,
    unsigned int line,
    const char*  function
);

#ifndef fail
#define fail(s) _fail(s, __FILE__, __LINE__, __func__)
#endif

void _assert_fail(
    const char*  assertion,
    const char*  file,
    unsigned int line,
    const char*  function
);

#ifndef assert
#define assert(expr) \
    if(!(expr)) _assert_fail(#expr, __FILE__, __LINE__, __FUNCTION__)
#endif

/* Create an assert that will trigger a compile error if it fails. */
#ifndef compile_assert
#define compile_assert(name, expr) \
        typedef int __assert_failed_##name[(expr) ? 1 : -1];
#endif

#else /* !DEBUG */

#include <libsel4_halt.h>

#ifndef fail
#define fail(s) halt()
#endif

#ifndef assert
#define assert(expr)
#endif

#ifndef compile_assert
#define compile_assert(name, expr)
#endif

#endif /* DEBUG */

#endif
