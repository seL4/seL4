/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once
#include <config.h>

#ifdef ENABLE_SMP_SUPPORT

#define LD_EX               "ldrex "
#define ST_EX               "strex "
#define OP_WIDTH

extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

/* Get current stack pointer */
static inline word_t getCurSP(void)
{
    word_t stack_address;
    asm("mov %[stack_address], %[currStackAddress]" : [stack_address] "=r"(stack_address) : [currStackAddress] "r"(&stack_address):);
    return stack_address;
}

static inline CONST cpu_id_t getCurrentCPUIndex(void)
{
    cpu_id_t cpu_id;
    word_t sp = getCurSP();

    sp -= (word_t) kernel_stack_alloc;
    cpu_id = sp >> CONFIG_KERNEL_STACK_BITS;
    return cpu_id;
}

#endif /* ENABLE_SMP_SUPPORT */

