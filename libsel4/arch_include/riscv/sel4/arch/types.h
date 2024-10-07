/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
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


/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */

typedef struct seL4_UserContext_ {
    seL4_Register pc;
    seL4_Register ra;
    seL4_Register sp;
    seL4_Register gp;

    seL4_Register s0;
    seL4_Register s1;
    seL4_Register s2;
    seL4_Register s3;
    seL4_Register s4;
    seL4_Register s5;
    seL4_Register s6;
    seL4_Register s7;
    seL4_Register s8;
    seL4_Register s9;
    seL4_Register s10;
    seL4_Register s11;

    seL4_Register a0;
    seL4_Register a1;
    seL4_Register a2;
    seL4_Register a3;
    seL4_Register a4;
    seL4_Register a5;
    seL4_Register a6;
    seL4_Register a7;

    seL4_Register t0;
    seL4_Register t1;
    seL4_Register t2;
    seL4_Register t3;
    seL4_Register t4;
    seL4_Register t5;
    seL4_Register t6;

    seL4_Register tp;
} seL4_UserContext;

typedef enum {
    seL4_RISCV_ExecuteNever = 0x1,
    seL4_RISCV_Default_VMAttributes = 0,
    SEL4_FORCE_LONG_ENUM(seL4_RISCV_VMAttributes)
} seL4_RISCV_VMAttributes;
