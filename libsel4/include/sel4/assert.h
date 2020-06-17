/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

/**
 * Declares macros and methods for sel4 specific assert and fail.
 *
 * These are unconditional, there are conditional versions of
 * these in debug_assert.h and are name seL4_DebugAssert and
 * sl4_DebugCompileTimeAsssert.
 */

#pragma once
/**
 * Hidden function, use the macros seL4_Fail or seL4_Assert.
 */
void __assert_fail(const char  *str, const char *file, int line, const char *function);

/**
 * If expr evaluates to false _seL4_Fail is called with the
 * expr as a string plus the file, line and function.
 */
#define seL4_Fail(s) __assert_fail(s, __FILE__, __LINE__, __func__)

/**
 * If expr evaluates to false _seL4_AssertFail is called with the
 * expr as a string plus the file, line and function.
 */
#define seL4_Assert(expr) \
    do { if (!(expr)) { __assert_fail(#expr, __FILE__, __LINE__, __FUNCTION__); } } while(0)

/**
 * An assert that tests that the expr is a compile time constant and
 * evaluates to true.
 *
 * This code is similar to compile_time_assert in libutils/include/utils/compile_time.h,
 * we may want to have only one. Also, using __COUNTER__ its not necessary to have name
 * for uniqueness so I've removed it, although maybe there is another reason for it, like
 * some compilers don't support __COUNTER__.
 */
#define seL4_CompileTimeAssert(expr) \
    extern char __seL4_CompileTimeAssertFailed_ ## __COUNTER__[__builtin_constant_p(expr) ? ((expr) ? 1 : -1) : -1] __attribute__((unused))

