/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once
#include <config.h>

#ifdef ENABLE_SMP_SUPPORT
extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

static inline CONST cpu_id_t getCurrentCPUIndex(void)
{
    char sp[8];
    word_t idx = (word_t)(sp - kernel_stack_alloc[0]);

    return idx >> CONFIG_KERNEL_STACK_BITS;
}

#endif /* ENABLE_SMP_SUPPORT */

