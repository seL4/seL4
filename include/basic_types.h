/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <stdint.h>
#include <arch/types.h>
#include <util.h>
#include <assert.h>

/* On RISC-V, the proofs expect wordBits to be a UL_CONST(), on other
 * architectures there is no such assumption. One day, the proofs should be
 * updated to expect a SEL4_WORD_CONST() everywhere.
 */
#ifdef CONFIG_ARCH_RISCV
#define wordBits UL_CONST(CONFIG_WORD_SIZE)
#else
#define wordBits CONFIG_WORD_SIZE
#endif
compile_assert(word_t_is_sane, 8 * sizeof(word_t) == wordBits)

#if CONFIG_WORD_SIZE == 32
#define wordRadix 5
#elif CONFIG_WORD_SIZE == 64
#define wordRadix 6
#else
#error "unsupported CONFIG_WORD_SIZE"
#endif /* CONFIG_WORD_SIZE */
compile_assert(wordRadix_is_sane, BIT(wordRadix) == CONFIG_WORD_SIZE)


/* arch/types.h is supposed to define word_t and _seL4_word_fmt */
#ifndef _seL4_word_fmt
#error "missing _seL4_word_fmt"
#endif

/* Define printf() format strings */
#define SEL4_PRIu_word  _macro_str_concat(_seL4_word_fmt, u)
#define SEL4_PRIx_word  _macro_str_concat(_seL4_word_fmt, x)
#define SEL4_PRI_word   SEL4_PRIu_word

/* The C parser from the verification toolchain requires declaring word_t
 * constants without casting integer values to word_t. Since the printf() format
 * specifiers are aligned with the C integer type suffixes, _seL4_word_fmt from
 * arch/types.h can be used here also.
 */
#define SEL4_WORD_CONST(x)  _macro_concat3(x, _seL4_word_fmt, u)

/* The type word_t_may_alias is equivalent to word_t except that we tell the
 * compiler that we may alias with any other type (similar to a char pointer)
 */
typedef word_t __attribute__((__may_alias__)) word_t_may_alias;

enum _bool {
    false = 0,
    true  = 1
};
typedef word_t bool_t;

/* for libsel4 headers that the kernel shares */
typedef uint8_t seL4_Uint8;
typedef uint16_t seL4_Uint16;
typedef uint32_t seL4_Uint32;
typedef word_t seL4_Word;
typedef cptr_t seL4_CPtr;
typedef node_id_t seL4_NodeId;
typedef paddr_t seL4_PAddr;
typedef dom_t seL4_Domain;
