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
    seL4_Word x3;
    seL4_Word x4;
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
    seL4_RISCV_Default_VMAttributes = 7,
    // RVTODO: understand and document these attributes

    seL4_RISCV_PTE_TYPE_TABLE = 0,
    seL4_RISCV_PTE_TYPE_TABLE_GLOBAL = 1,
    seL4_RISCV_PTE_TYPE_URX_SR = 2,
    seL4_RISCV_PTE_TYPE_URWX_SRW = 3,
    seL4_RISCV_PTE_TYPE_UR_SR = 4,
    seL4_RISCV_PTE_TYPE_URW_SRW = 5,
    seL4_RISCV_PTE_TYPE_URX_SRX = 6,
    seL4_RISCV_PTE_TYPE_URWX_SRWX = 7,
    seL4_RISCV_PTE_TYPE_SR = 8,
    seL4_RISCV_PTE_TYPE_SRW = 9,
    seL4_RISCV_PTE_TYPE_SRX = 10,
    seL4_RISCV_PTE_TYPE_SRWX = 11,
    seL4_RISCV_PTE_TYPE_SR_GLOBAL = 12,
    seL4_RISCV_PTE_TYPE_SRW_GLOBAL = 13,
    seL4_RISCV_PTE_TYPE_SRX_GLOBAL = 14,
    seL4_RISCV_PTE_TYPE_SRWX_GLOBAL = 15,
    SEL4_FORCE_LONG_ENUM(seL4_RISCV_VMAttributes)
} seL4_RISCV_VMAttributes;

#endif
