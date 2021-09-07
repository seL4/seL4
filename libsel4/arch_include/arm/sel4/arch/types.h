/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/macros.h>
#include <sel4/simple_types.h>
#include <sel4/sel4_arch/types.h>

typedef seL4_CPtr seL4_ARM_Page;
typedef seL4_CPtr seL4_ARM_PageTable;
typedef seL4_CPtr seL4_ARM_PageDirectory;
typedef seL4_CPtr seL4_ARM_ASIDControl;
typedef seL4_CPtr seL4_ARM_ASIDPool;
typedef seL4_CPtr seL4_ARM_VCPU;
typedef seL4_CPtr seL4_ARM_IOSpace;
typedef seL4_CPtr seL4_ARM_IOPageTable;
typedef seL4_CPtr seL4_ARM_SIDControl;
typedef seL4_CPtr seL4_ARM_SID;
typedef seL4_CPtr seL4_ARM_CBControl;
typedef seL4_CPtr seL4_ARM_CB;

typedef enum {
    seL4_ARM_PageCacheable = 0x01,
    seL4_ARM_ParityEnabled = 0x02,
    seL4_ARM_Default_VMAttributes = 0x03,
    seL4_ARM_ExecuteNever  = 0x04,
    /* seL4_ARM_PageCacheable | seL4_ARM_ParityEnabled */
    SEL4_FORCE_LONG_ENUM(seL4_ARM_VMAttributes),
} seL4_ARM_VMAttributes;

typedef enum {
    seL4_ARM_CacheI   = 1,
    seL4_ARM_CacheD   = 2,
    seL4_ARM_CacheID  = 3,
    SEL4_FORCE_LONG_ENUM(seL4_ARM_CacheType),
} seL4_ARM_CacheType;

