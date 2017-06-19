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

#ifndef __LIBSEL4_SIMPLE_TYPES_H
#define __LIBSEL4_SIMPLE_TYPES_H

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

#endif // __LIBSEL4_SIMPLE_TYPES_H
