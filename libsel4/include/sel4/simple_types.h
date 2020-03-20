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

