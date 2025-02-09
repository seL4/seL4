/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/macros.h>

/*
 * Data       | short | int | long | long | size_t | void*  |  notes
 * model      |       |     |      | long |        |
 * -----------+-------+-----+------+------+--------+----------------
 * IP16       | 16    | 16  | 32   | 64   | 16     | 16     | MS-DOS SMALL memory model
 * LP32       | 16    | 16  | 32   | 64   | 32     | 32     | MS-DOS LARGE memory model
 * ILP32      | 16    | 32  | 32   | 64   | 32     | 32     | common for a 32-bit OS
 * LLP64      | 16    | 32  | 32   | 64   | 64     | 64     | 64-bit Windows, VC++, MinGW
 * LP64       | 16    | 32  | 64   | 64   | 64     | 64     | most 64-bit Unix systems
 * ILP64      | 16    | 64  | 64   | 64   | 64     | 64     | SPARC64, Solaris
 * SILP64     | 64    | 64  | 64   | 64   | 64     | 64     | UNICOS
 * IL32PC64   | 16    | 32  | 32   | 64   | 32     | 64     | 32-bit CHERI OS
 * L64PC128   | 16    | 32  | 64   | 64   | 64     | 128    | 64-bit CHERI OS
 *
 * libsel4 requires ILP32 on 32-bit systems and and LP64 on 64-bit systems
 * On CHERI-enabled seL4, IL32P64 is required for 32-bit systems and
 * L64PC128 for 64-bit.
 */

/* Get the architectural definitions and types */
#include <sel4/arch/simple_types.h>

#define seL4_integer_size_assert(_type_, _size_) \
    SEL4_COMPILE_ASSERT( \
        sizeof_##_type_##_is_##_size_##_byte, \
        sizeof(_type_) == _size_ )


/* C99 defines that this is at least 8-bit */
#define _seL4_int8_type             char
typedef signed _seL4_int8_type      seL4_Int8;
typedef unsigned _seL4_int8_type    seL4_Uint8;

seL4_integer_size_assert(seL4_Int8, 1)
seL4_integer_size_assert(seL4_Uint8, 1)


/* C99 defines that a 'short int' is at least 16 bits. Note that 'short' is
 * no basic C type, but only a type modifier. If nothing else is given, the
 * default type is 'int' and thus it is often omitted.
 */
#define _seL4_int16_type            short int
typedef signed _seL4_int16_type     seL4_Int16;
typedef unsigned _seL4_int16_type   seL4_Uint16;

seL4_integer_size_assert(seL4_Int16, 2)
seL4_integer_size_assert(seL4_Uint16, 2)


/* C99 does not define that an 'int' is 32-bit, it just has to be no smaller
 * than a 'short int'. We require ILP32 on 32-bit systems and and LP64 on 64-bit
 * systems, so 'int' is 32-bits. */
#define _seL4_int32_type            int
typedef signed _seL4_int32_type     seL4_Int32;
typedef unsigned _seL4_int32_type   seL4_Uint32;

seL4_integer_size_assert(seL4_Int32, 4)
seL4_integer_size_assert(seL4_Uint32, 4)


/* There is no standard 64-bit type in C, so the architecture specific headers
 * must define what to use. C99 just defines that 'long' is at least 32 bits and
 * 'long long' at least 64-bit. Note that neither 'long' nor 'long long' are
 * basic C types, they are only type modifiers. If nothing else is given, the
 * default base type is 'int' and thus it is often omitted.
 */
#if defined(SEL4_INT64_IS_LONG)
#define _seL4_int64_type    long int
#define _seL4_int64_fmt     l  // printf() formatting, integer suffix
#elif defined(SEL4_INT64_IS_LONG_LONG)
#define _seL4_int64_type    long long int
#define _seL4_int64_fmt     ll  // printf() formatting, integer suffix
#else
#error missing definition for 64-bit types
#endif

typedef signed _seL4_int64_type     seL4_Int64;
typedef unsigned _seL4_int64_type   seL4_Uint64;

/* Define printf() format specifiers for seL4_Int64 and seL4_Uint64. The helper
 * macros ensure these format strings are atoms. The macros passed are evaluated
 * before the concatenation and stringizing happens.
 */
#define _macro_str_concat_helper2(x)    #x
#define _macro_str_concat_helper1(x,y)  _macro_str_concat_helper2(x ## y)
#define _macro_str_concat(x,y)          _macro_str_concat_helper1(x,y)

#define SEL4_PRId64     _macro_str_concat(_seL4_int64_fmt, d)
#define SEL4_PRIi64     _macro_str_concat(_seL4_int64_fmt, i)
#define SEL4_PRIu64     _macro_str_concat(_seL4_int64_fmt, u)
#define SEL4_PRIx64     _macro_str_concat(_seL4_int64_fmt, x)

/* Define boolean type and true/false */
#define seL4_True   1
#define seL4_False  0
typedef seL4_Int8   seL4_Bool;

/* Define seL4_Null */
#ifdef __cplusplus
#define seL4_Null   0L
#else
#define seL4_Null   ((void*)0)
#endif // __cplusplus

/* Define seL4_Word */
#if defined(SEL4_WORD_IS_UINT32)
typedef seL4_Uint32 seL4_Word;
#define _seL4_word_fmt /* empty */
#elif defined(SEL4_WORD_IS_UINT64)
typedef seL4_Uint64 seL4_Word;
#define _seL4_word_fmt _seL4_int64_fmt
#else
#error missing definition for SEL4_WORD type
#endif
/* printf() format specifiers for seL4_Word */
#define SEL4_PRId_word  _macro_str_concat(_seL4_word_fmt, d)
#define SEL4_PRIi_word  _macro_str_concat(_seL4_word_fmt, i)
#define SEL4_PRIu_word  _macro_str_concat(_seL4_word_fmt, u)
#define SEL4_PRIx_word  _macro_str_concat(_seL4_word_fmt, x)
#define SEL4_PRI_word   SEL4_PRIu_word

typedef seL4_Word seL4_CPtr;

#if defined(CONFIG_HAVE_CHERI)
typedef __uintcap_t seL4_Register;
#else
typedef seL4_Word seL4_Register;
#endif

/* sanity check that the seL4_Word matches the definitions of the constants */
#include <sel4/sel4_arch/constants.h>

SEL4_COMPILE_ASSERT(
    seL4_WordSizeBits_matches,
    sizeof(seL4_Word) == (1u << seL4_WordSizeBits))

SEL4_COMPILE_ASSERT(
    seL4_WordBits_matches,
    8 * sizeof(seL4_Word) == seL4_WordBits)
