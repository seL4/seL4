/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/macros.h>

/*
 * Data   | short | int | long | long | void*  | notes
 * model  |       |     |      | long | size_t |
 * -------+-------+-----+------+------+--------+----------------
 * IP16   | 16    | 16  | 32   | 64   | 16     | MS-DOS SMALL memory model
 * LP32   | 16    | 16  | 32   | 64   | 32     | MS-DOS LARGE memory model
 * ILP32  | 16    | 32  | 32   | 64   | 32     | common for a 32-bit OS
 * LLP64  | 16    | 32  | 32   | 64   | 64     | 64-bit Windows, VC++, MinGW
 * LP64   | 16    | 32  | 64   | 64   | 64     | most 64-bit Unix systems
 * ILP64  | 16    | 64  | 64   | 64   | 64     | SPARC64, Solaris
 * SILP64 | 64    | 64  | 64   | 64   | 64     | UNICOS
 *
 * libsel4 requires ILP32 on 32-bit systems and and LP64 on 64-bit systems
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
#elif defined(SEL4_INT64_IS_LONG_LONG)
#define _seL4_int64_type    long long int
#else
#error missing definition for 64-bit types
#endif

typedef signed _seL4_int64_type     seL4_Int64;
typedef unsigned _seL4_int64_type   seL4_Uint64;

seL4_integer_size_assert(seL4_Int64, 8)
seL4_integer_size_assert(seL4_Uint64, 8)


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
#elif defined(SEL4_WORD_IS_UINT64)
typedef seL4_Uint64 seL4_Word;
#else
#error missing definition for SEL4_WORD type
#endif
