/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_TYPES_H
#define __LIBSEL4_ARCH_TYPES_H

#include <autoconf.h>
#include <sel4/simple_types.h>

/* for x86-64, the large page size is 2 MiB and huge page size is 1 GiB */
/* the seL4_WordBits is still 32, avoiding breaking user-libraries */
#define seL4_WordBits           64 //32
#define seL4_PageBits           12
#define seL4_SlotBits           5
#define seL4_TCBBits            11
#define seL4_EndpointBits       4
#define seL4_NotificationBits   5
#define seL4_PageTableBits      12
#define seL4_PageDirBits        12
#define seL4_PDPTBits           12
#define seL4_PML4Bits           12
#define seL4_IOPageTableBits    12
#define seL4_LargePageBits      21
#define seL4_HugePageBits       30

typedef seL4_Uint64 seL4_Word;
typedef seL4_Word seL4_CPtr;

typedef seL4_CPtr seL4_IA32_ASIDControl;
typedef seL4_CPtr seL4_IA32_ASIDPool;
typedef seL4_CPtr seL4_IA32_IOSpace;
typedef seL4_CPtr seL4_X86_IOPort;
typedef seL4_CPtr seL4_IA32_Page;
typedef seL4_CPtr seL4_IA32_PML4;
typedef seL4_CPtr seL4_IA32_PDPT;
typedef seL4_CPtr seL4_IA32_PageDirectory;
typedef seL4_CPtr seL4_IA32_PageTable;
typedef seL4_CPtr seL4_IA32_IOPageTable;

/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */

typedef struct seL4_UserContext_ {
    seL4_Word rip, rsp, rflags, rax, rbx, rcx, rdx, rsi, rdi, rbp,
              r8, r9, r10, r11, r12, r13, r14, r15;
    seL4_Word tls_base, fs, gs;
} seL4_UserContext;

typedef enum {
    seL4_IA32_Default_VMAttributes = 0,
    seL4_IA32_WriteBack = 0,
    seL4_IA32_WriteThrough = 1,
    seL4_IA32_CacheDisabled = 2,
    seL4_IA32_Uncacheable = 3,
    seL4_IA32_WriteCombining = 4,
    SEL4_FORCE_LONG_ENUM(seL4_IA32_VMAttributes),
} seL4_IA32_VMAttributes;

#endif
