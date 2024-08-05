/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/simple_types.h>

typedef seL4_CPtr seL4_X64_PML4;

/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */

typedef struct seL4_UserContext_ {
    seL4_Word rip, rsp, rflags, rax, rbx, rcx, rdx, rsi, rdi, rbp,
              r8, r9, r10, r11, r12, r13, r14, r15;
    seL4_Word fs_base, gs_base;
} seL4_UserContext;
