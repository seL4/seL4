/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/simple_types.h>

/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */
typedef struct seL4_UserContext_ {
    /* frameRegisters */
    seL4_Word eip, esp, eflags, eax, ebx, ecx, edx, esi, edi, ebp;
    /* gpRegisters */
    seL4_Word fs_base, gs_base;
} seL4_UserContext;
