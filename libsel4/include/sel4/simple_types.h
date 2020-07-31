/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

/* Get the architectural types seL4_{u}int{N} */
#include <sel4/arch/simple_types.h>
#include <sel4/sel4_arch/simple_types.h>

/* Define boolean type and true/false */
#define seL4_True 1
#define seL4_False 0
typedef seL4_Int8 seL4_Bool;

/* Define seL4_Null */
#ifdef __cplusplus
#define seL4_Null 0L
#else
#define seL4_Null ((void*)0)
#endif // __cplusplus

/* Sign extension helper for bitfields, one per base type.  size is the
 * allocated size of the bitfield, not including grain; grain is the number
 * of unstored low zero bits (for field_high).  The inner if is recognized
 * as a single arithmetic shift by gcc and clang. */
#define SEL4_BITFIELD_HELPER(f_name, bits, s_type, u_type) \
static inline u_type f_name(int size, int grain, int issigned, u_type value) \
{ \
    value = (value >> grain) << (bits - size); \
    if (issigned) { \
        if (value >> (bits - 1)) { \
            value = ~(u_type)((s_type)~value >> (bits - grain - size)); \
        } else { \
            value = (u_type)((s_type)value >> (bits - grain - size)); \
        } \
    } else { \
        value = value >> (bits - grain - size); \
    } \
    return value; \
}
SEL4_BITFIELD_HELPER(seL4_extend_bitfield_8, 8, seL4_Int8, seL4_Uint8)
SEL4_BITFIELD_HELPER(seL4_extend_bitfield_16, 16, seL4_Int16, seL4_Uint16)
SEL4_BITFIELD_HELPER(seL4_extend_bitfield_32, 32, seL4_Int32, seL4_Uint32)
SEL4_BITFIELD_HELPER(seL4_extend_bitfield_64, 64, seL4_Int64, seL4_Uint64)
