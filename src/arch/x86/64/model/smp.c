/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <mode/model/smp.h>

#ifdef ENABLE_SMP_SUPPORT

nodeInfo_t node_info[CONFIG_MAX_NUM_NODES] ALIGN(L1_CACHE_LINE_SIZE) VISIBLE;
char nodeSkimScratch[CONFIG_MAX_NUM_NODES][sizeof(nodeInfo_t)] ALIGN(L1_CACHE_LINE_SIZE);
extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

BOOT_CODE void mode_init_tls(cpu_id_t cpu_index)
{
    node_info[cpu_index].stackTop = kernel_stack_alloc[cpu_index + 1];
    node_info[cpu_index].irqStack = &x64KSIRQStack[cpu_index][0];
    node_info[cpu_index].index = cpu_index;
    x86_wrmsr(IA32_KERNEL_GS_BASE_MSR, (word_t)&node_info[cpu_index]);
    swapgs();
}

#endif /* ENABLE_SMP_SUPPORT */
