/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

/**
 * Declares macros and methods which are conditional based
 * on NDEBUG. If  NDEBUG is defined then seL4_DebugAssert
 * is a NOP, otherwise it invokes the unconditional version
 * in sel4/assert.h.
 */
#pragma once

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

