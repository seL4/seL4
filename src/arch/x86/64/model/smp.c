/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#include <config.h>
#include <mode/model/smp.h>

#if CONFIG_MAX_NUM_NODES > 1

nodeInfo_t node_info[CONFIG_MAX_NUM_NODES];
extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(seL4_PageBits)];

BOOT_CODE void
mode_init_tls(cpu_id_t cpu_index)
{
    node_info[cpu_index].stackTop = kernel_stack_alloc[cpu_index + 1];
    node_info[cpu_index].irqStack = &MODE_NODE_STATE_ON_CORE(x64KSIRQStack, cpu_index)[0];
    node_info[cpu_index].index = cpu_index;
    x86_wrmsr(IA32_KERNEL_GS_BASE_MSR, (word_t)&node_info[cpu_index]);
    swapgs();
}

#endif /* CONFIG_MAX_NUM_NODES > 1 */
