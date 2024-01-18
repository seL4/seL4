/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <stdint.h>
#include <arch/types.h>

/* arch/types.h is supposed to define word_t and _seL4_word_fmt */
#ifndef _seL4_word_fmt
#error "missing _seL4_word_fmt"
#endif

#define SEL4_PRIu_word  _macro_str_concat(_seL4_word_fmt, u)
#define SEL4_PRIx_word  _macro_str_concat(_seL4_word_fmt, x)
#define SEL4_PRI_word   SEL4_PRIu_word

/* The C parser from the verification toolchain requires declaring word_t
 * constants without casting integer values to word_t. Since the printf() format
 * specifiers are aligned with the C integer type suffixes, _seL4_word_fmt can
 * be used there also.
 */
#define SEL4_WORD_CONST(x)  _macro_concat3(x, _seL4_word_fmt, u)


enum _bool {
    false = 0,
    true  = 1
};
typedef word_t bool_t;

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
