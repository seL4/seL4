/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/macros.h>
#include <sel4/simple_types.h>
#include <sel4/sel4_arch/types.h>

typedef seL4_CPtr seL4_RISCV_Page;
typedef seL4_CPtr seL4_RISCV_PageTable;
typedef seL4_CPtr seL4_RISCV_ASIDControl;
typedef seL4_CPtr seL4_RISCV_ASIDPool;


#define seL4_EndpointBits     4
/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */

typedef struct seL4_UserContext_ {
    seL4_Word pc;
    seL4_Word ra;
    seL4_Word sp;
    seL4_Word gp;

    seL4_Word s0;
    seL4_Word s1;
    seL4_Word s2;
    seL4_Word s3;
    seL4_Word s4;
    seL4_Word s5;
    seL4_Word s6;
    seL4_Word s7;
    seL4_Word s8;
    seL4_Word s9;
    seL4_Word s10;
    seL4_Word s11;

    seL4_Word a0;
    seL4_Word a1;
    seL4_Word a2;
    seL4_Word a3;
    seL4_Word a4;
    seL4_Word a5;
    seL4_Word a6;
    seL4_Word a7;

    seL4_Word t0;
    seL4_Word t1;
    seL4_Word t2;
    seL4_Word t3;
    seL4_Word t4;
    seL4_Word t5;
    seL4_Word t6;

    seL4_Word tp;
} seL4_UserContext;

typedef enum {
    seL4_RISCV_ExecuteNever = 0x1,
    seL4_RISCV_Default_VMAttributes = 0,
    SEL4_FORCE_LONG_ENUM(seL4_RISCV_VMAttributes)
} seL4_RISCV_VMAttributes;
