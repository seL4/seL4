/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __LIBSEL4_ARCH_TYPES_H
#define __LIBSEL4_ARCH_TYPES_H

#include <autoconf.h>
#include <sel4/simple_types.h>
#include <sel4/sel4_arch/types.h>

typedef seL4_Word seL4_CPtr;

typedef seL4_CPtr seL4_RISCV_Page;
typedef seL4_CPtr seL4_RISCV_PageTable;
typedef seL4_CPtr seL4_RISCV_ASIDControl;
typedef seL4_CPtr seL4_RISCV_ASIDPool;

typedef seL4_Word seL4_NodeId;
typedef seL4_Word seL4_PAddr;
typedef seL4_Word seL4_Domain;

#define seL4_EndpointBits     4
/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */

typedef struct seL4_UserContext_ {
    seL4_Word pc;
    seL4_Word ra;
    seL4_Word sp;
    seL4_Word gp;
    seL4_Word tp;
    seL4_Word t0;
    seL4_Word t1;
    seL4_Word t2;
    seL4_Word s0;
    seL4_Word s1;
    seL4_Word a0;
    seL4_Word a1;
    seL4_Word a2;
    seL4_Word a3;
    seL4_Word a4;
    seL4_Word a5;
    seL4_Word a6;
} seL4_UserContext;

typedef enum {
    seL4_RISCV_ExecuteNever = 0x1,
    seL4_RISCV_Default_VMAttributes = 0,
    SEL4_FORCE_LONG_ENUM(seL4_RISCV_VMAttributes)
} seL4_RISCV_VMAttributes;

#endif
