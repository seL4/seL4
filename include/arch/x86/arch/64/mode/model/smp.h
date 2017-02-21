/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __MODE_MODEL_SMP_H_
#define __MODE_MODEL_SMP_H_

#include <config.h>

#if CONFIG_MAX_NUM_NODES > 1

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
} nodeInfo_t;

extern nodeInfo_t node_info[CONFIG_MAX_NUM_NODES];

static inline CONST cpu_id_t getCurrentCPUIndex(void)
{
    cpu_id_t index;
    asm ("movq %%gs:%c[offset], %[result]"
         : [result] "=r" (index)
         : [offset] "i" (OFFSETOF(nodeInfo_t, index)));
    return index;
}

BOOT_CODE void
mode_init_tls(cpu_id_t cpu_index);

#endif /* CONFIG_MAX_NUM_NODES > 1 */

#endif /* __MODE_MODEL_SMP_H_ */
