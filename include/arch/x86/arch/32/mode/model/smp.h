/*
 * Copyright 2017, Data61
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

#ifdef ENABLE_SMP_SUPPORT

extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

/* Get current stack pointer */
static inline void*
getCurESP(void)
{
    word_t stack;
    void *result;
    asm ("movl %[stack_address], %[result]" : [result] "=r"(result) : [stack_address] "r"(&stack));
    return result;
}

static inline CONST cpu_id_t
getCurrentCPUIndex(void)
{
    cpu_id_t cpu_id;
    uint32_t esp = (uint32_t)getCurESP();

    esp -= (uint32_t)kernel_stack_alloc;
    cpu_id = esp >> CONFIG_KERNEL_STACK_BITS;
    return cpu_id;
}

static inline BOOT_CODE void
mode_init_tls(cpu_id_t cpu_index)
{
    /* Nothing to be done on ia32 */
}

#endif /* ENABLE_SMP_SUPPORT */

#endif /* __MODE_MODEL_SMP_H_ */
