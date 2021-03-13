/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/simple_types.h>

typedef struct seL4_UserContext_ {
    /* frame registers */
    seL4_Word pc, sp, cpsr, r0, r1, r8, r9, r10, r11, r12;
    /* other integer registers */
    seL4_Word r2, r3, r4, r5, r6, r7, r14;
    /* Thread ID registers */
    seL4_Word tpidrurw, tpidruro;
} seL4_UserContext;
