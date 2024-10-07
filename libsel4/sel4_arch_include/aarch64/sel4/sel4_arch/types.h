/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/simple_types.h>

typedef seL4_CPtr seL4_ARM_PageUpperDirectory;
typedef seL4_CPtr seL4_ARM_PageGlobalDirectory;
/* whether the VSpace refers to a PageUpperDirectory or PageGlobalDirectory directly
 * depends on the physical address size */
typedef seL4_CPtr seL4_ARM_VSpace;

typedef struct seL4_UserContext_ {
    /* frame registers */
    /* Registers that could hold pointers (and CHERI capabilities, when enabled) */
    seL4_Register pc, sp, x0, x1, x2, x3, x4, x5, x6, x7, x8, x16, x17, x18, x29, x30;
    /* other pointer registers */
    seL4_Register x9, x10, x11, x12, x13, x14, x15, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28;
    /* Thread ID registers */
    seL4_Register tpidr_el0, tpidrro_el0;
    seL4_Word spsr; /* Status register - 64-bit integer */

} seL4_UserContext;

typedef struct seL4_ARM_SMCContext_ {
    /* register arguments */
    seL4_Register x0, x1, x2, x3, x4, x5, x6, x7;
} seL4_ARM_SMCContext;
