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

#ifndef __LIBSEL4_SEL4_ARCH_TYPES_H
#define __LIBSEL4_SEL4_ARCH_TYPES_H

#include <autoconf.h>

typedef seL4_Uint32 seL4_Word;
typedef seL4_Word seL4_NodeId;
typedef seL4_Word seL4_PAddr;
typedef seL4_Word seL4_Domain;

typedef seL4_Word seL4_CPtr;

/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */
typedef struct seL4_UserContext_ {
    /* frameRegisters */
    seL4_Word eip, esp, eflags, eax, ebx, ecx, edx, esi, edi, ebp;
    /* gpRegisters */
    seL4_Word tls_base, fs, gs;
} seL4_UserContext;

#endif
