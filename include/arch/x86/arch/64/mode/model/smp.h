/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef ENABLE_SMP_SUPPORT

typedef struct nodeInfo {
    void *stackTop;
    void *irqStack;
    /* This is the address (+8 bytes) of the 'Error' register of the user
     * context of the current thread. This address is designed so that the
     * 'syscall' trap code can do
     * movq %gs:16, %rsp
     * pushq $-1
     * pushq %rcx (rcx holds NextIP)
     * pushq %r11 (r11 holds RFLAGS)
     * etc etc
     * This value needs to be updated every time we switch to the user
     */
    word_t currentThreadUserContext;
    cpu_id_t index;
    PAD_TO_NEXT_CACHE_LN(sizeof(void *) + sizeof(void *) + sizeof(word_t) + sizeof(cpu_id_t));
} nodeInfo_t;
compile_assert(nodeInfoIsCacheSized, (sizeof(nodeInfo_t) % L1_CACHE_LINE_SIZE) == 0)

extern nodeInfo_t node_info[CONFIG_MAX_NUM_NODES] ALIGN(L1_CACHE_LINE_SIZE);

#ifdef CONFIG_KERNEL_SKIM_WINDOW
/* we only need 1 word of scratch space, which we know we will at least get by the size
 8 of the nodeInfo_t struct so we define this array as char to ensure it is sized correctly.
 We need each element of the array to be precisely the same size as each element of node_info
 so that the offset between the element of each array is a constant */
extern char nodeSkimScratch[CONFIG_MAX_NUM_NODES][sizeof(nodeInfo_t)] ALIGN(L1_CACHE_LINE_SIZE) VISIBLE SKIM_BSS;
compile_assert(nodeInfoAndScratchSameSize, sizeof(node_info) == sizeof(nodeSkimScratch))
/* this will be declared in the linker script as the offset between node_info and
 * nodeSkimScratch */
extern char nodeSkimScratchOffset[];
#endif

static inline CONST cpu_id_t getCurrentCPUIndex(void)
{
    cpu_id_t index;
    asm("movq %%gs:%c[offset], %[result]"
        : [result] "=r"(index)
        : [offset] "i"(OFFSETOF(nodeInfo_t, index)));
    return index;
}

BOOT_CODE void
mode_init_tls(cpu_id_t cpu_index);

#endif /* ENABLE_SMP_SUPPORT */
