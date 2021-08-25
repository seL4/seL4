/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <kernel/stack.h>

#ifdef ENABLE_SMP_SUPPORT

#define LD_EX               "ldxr "
#define ST_EX               "stxr "
#define OP_WIDTH            "w"

extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];
compile_assert(kernel_stack_4k_aligned, KERNEL_STACK_ALIGNMENT == 4096)

#define CPUID_MASK  (KERNEL_STACK_ALIGNMENT - 1)

static inline CONST cpu_id_t
getCurrentCPUIndex(void)
{
    cpu_id_t id;
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        asm volatile("mrs %0, tpidr_el2" : "=r"(id));
    } else {
        asm volatile("mrs %0, tpidr_el1" : "=r"(id));
    }
    return (id & CPUID_MASK);
}

#endif /* ENABLE_SMP_SUPPORT */
