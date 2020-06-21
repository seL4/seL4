/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#pragma once
#include <config.h>

#ifdef ENABLE_SMP_SUPPORT

extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

/* Get current stack pointer */
static inline void *getCurESP(void)
{
    word_t stack;
    void *result;
    asm("movl %[stack_address], %[result]" : [result] "=r"(result) : [stack_address] "r"(&stack));
    return result;
}

static inline CONST cpu_id_t getCurrentCPUIndex(void)
{
    cpu_id_t cpu_id;
    uint32_t esp = (uint32_t)getCurESP();

    esp -= (uint32_t)kernel_stack_alloc;
    cpu_id = esp >> CONFIG_KERNEL_STACK_BITS;
    return cpu_id;
}

static inline BOOT_CODE void mode_init_tls(cpu_id_t cpu_index)
{
    /* Nothing to be done on ia32 */
}

#endif /* ENABLE_SMP_SUPPORT */

