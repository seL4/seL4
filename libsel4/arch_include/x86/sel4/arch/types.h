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
#include <sel4/sel4_arch/types.h>

typedef seL4_CPtr seL4_IA32_ASIDControl;
typedef seL4_CPtr seL4_IA32_ASIDPool;
typedef seL4_CPtr seL4_IA32_IOSpace;
typedef seL4_CPtr seL4_IA32_IOPort;
typedef seL4_CPtr seL4_IA32_Page;
typedef seL4_CPtr seL4_IA32_PDPT;
typedef seL4_CPtr seL4_IA32_PageDirectory;
typedef seL4_CPtr seL4_IA32_PageTable;
typedef seL4_CPtr seL4_IA32_IOPageTable;

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
