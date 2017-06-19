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

/**
 * Declares macros and methods which are conditional based
 * on NDEBUG. If  NDEBUG is defined then seL4_DebugAssert
 * is a NOP, otherwise it invokes the unconditional version
 * in sel4/assert.h.
 */
#ifndef __LIBSEL4_DEBUG_ASSERT_H
#define __LIBSEL4_DEBUG_ASSERT_H

#ifdef NDEBUG

/** NDEBUG is defined do nothing */
#define seL4_DebugAssert(expr) ((void)(0))

#else // NDEBUG is not defined

#include <sel4/assert.h>

/**
 * NDEBUG is not defined invoke seL4_Assert(expr).
 */
#define seL4_DebugAssert(expr) seL4_Assert(expr)

#endif // NDEBUG

#endif // __LIBSEL4_DEBUG_ASSERT_H
