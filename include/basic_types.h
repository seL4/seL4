/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <stdint.h>
#include <arch/types.h>

/* arch/types.h is supposed to define word_t and _seL4_word_fmt */
#ifndef _seL4_word_fmt
#error "missing _seL4_word_fmt"
#endif

/* Using multiple macro layers may look strange, but this is required to make
 * the preprocessor fully evaluate all macro parameters first and then pass the
 * result as parameter to the next macro layer. This allows passing macros as
 * parameters also, and not just plain strings. The final concatenation will
 * always be from the strings behind all macros then - and not the macro names
 * that are passed as parameters.
 */
#define _macro_concat_helper2(x,y,z)    x ## y ## z
#define _macro_concat_helper(x,y,z)     _macro_concat_helper2(x,y,z)

#define _macro_str_concat_helper2(x)    #x
#define _macro_str_concat_helper1(x,y)  _macro_str_concat_helper2(x ## y)
#define _macro_str_concat(x,y)          _macro_str_concat_helper1(x,y)

#define SEL4_PRIu_word  _macro_str_concat(_seL4_word_fmt, u)
#define SEL4_PRIx_word  _macro_str_concat(_seL4_word_fmt, x)
#define SEL4_PRI_word   SEL4_PRIu_word

/* The C parser from the verification toolchain requires declaring word_t
 * constants without casting integer values to word_t. Since the printf() format
 * specifiers are aligned with the C integer type suffixes, _seL4_word_fmt can
 * be used there also.
 */
#define SEL4_WORD_CONST(x)  _macro_concat_helper(x, _seL4_word_fmt, u)


enum _bool {
    false = 0,
    true  = 1
};
typedef word_t bool_t;

/**
 * A region [start..end) of kernel-virtual memory.
 *
 * Empty when start == end. If end < start, the region wraps around, that is,
 * it represents the addresses in the set [start..-1] union [0..end). This is
 * possible after address translation and fine for e.g. device memory regions.
 */
typedef struct region {
    pptr_t start; /* inclusive */
    pptr_t end;   /* exclusive */
} region_t;

/** A region [start..end) of physical memory addresses. */
typedef struct p_region {
    paddr_t start; /* inclusive */
    paddr_t end;   /* exclusive */
} p_region_t;

/** A region [start..end) of user-virtual addresses. */
typedef struct v_region {
    vptr_t start; /* inclusive */
    vptr_t end;   /* exclusive */
} v_region_t;

#define REG_EMPTY (region_t){ .start = 0, .end = 0 }
#define P_REG_EMPTY (p_region_t){ .start = 0, .end = 0 }

/* equivalent to a word_t except that we tell the compiler that we may alias with
 * any other type (similar to a char pointer) */
typedef word_t __attribute__((__may_alias__)) word_t_may_alias;

/* for libsel4 headers that the kernel shares */
typedef uint8_t seL4_Uint8;
typedef uint16_t seL4_Uint16;
typedef uint32_t seL4_Uint32;
typedef word_t seL4_Word;
typedef cptr_t seL4_CPtr;
typedef node_id_t seL4_NodeId;
typedef paddr_t seL4_PAddr;
typedef dom_t seL4_Domain;
