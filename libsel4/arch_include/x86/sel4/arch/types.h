/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/macros.h>
#include <sel4/simple_types.h>
#include <sel4/sel4_arch/types.h>

typedef seL4_CPtr seL4_X86_ASIDControl;
typedef seL4_CPtr seL4_X86_ASIDPool;
typedef seL4_CPtr seL4_X86_IOSpace;
typedef seL4_CPtr seL4_X86_IOPort;
typedef seL4_CPtr seL4_X86_IOPortControl;
typedef seL4_CPtr seL4_X86_Page;
typedef seL4_CPtr seL4_X86_PDPT;
typedef seL4_CPtr seL4_X86_PageDirectory;
typedef seL4_CPtr seL4_X86_PageTable;
typedef seL4_CPtr seL4_X86_IOPageTable;
typedef seL4_CPtr seL4_X86_EPTPML4;
typedef seL4_CPtr seL4_X86_EPTPDPT;
typedef seL4_CPtr seL4_X86_EPTPD;
typedef seL4_CPtr seL4_X86_EPTPT;
typedef seL4_CPtr seL4_X86_VCPU;

typedef enum {
    seL4_X86_Default_VMAttributes = 0,
    seL4_X86_WriteBack = 0,
    seL4_X86_WriteThrough = 1,
    seL4_X86_CacheDisabled = 2,
    seL4_X86_Uncacheable = 3,
    seL4_X86_WriteCombining = 4,
    SEL4_FORCE_LONG_ENUM(seL4_X86_VMAttributes),
} seL4_X86_VMAttributes;

typedef enum {
    seL4_X86_EPT_Uncached_VMAttributes = 6,
    seL4_X86_EPT_Uncacheable = 0,
    seL4_X86_EPT_WriteCombining = 1,
    seL4_X86_EPT_WriteThrough = 4,
    seL4_X86_EPT_WriteProtected = 5,
    seL4_X86_EPT_WriteBack = 6,
    seL4_X86_EPT_Default_VMAttributes = 6,
    SEL4_FORCE_LONG_ENUM(seL4_X86_EPT_VMAttributes),
} seL4_X86_EPT_VMAttributes;

typedef struct seL4_VCPUContext_ {
    seL4_Word eax, ebx, ecx, edx, esi, edi, ebp;
} seL4_VCPUContext;
