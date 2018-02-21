/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 */

#ifndef __MODE_SMP_H_
#define __MODE_SMP_H_

#include <config.h>

#ifdef ENABLE_SMP_SUPPORT
extern char kernel_stack_alloc[CONFIG_MAX_NUM_NODES][BIT(CONFIG_KERNEL_STACK_BITS)];

/* Get current stack pointer */
static inline word_t
getCurSP(void)
{
    word_t stack_address;
    asm ("mv %[stack_address], %[currStackAddress]" : [stack_address] "=r"(stack_address) : [currStackAddress] "r" (&stack_address):);
    return stack_address;
}

static inline CONST cpu_id_t
getCurrentCPUIndex(void)
{
    cpu_id_t cpu_id;
    word_t sp = getCurSP();

    sp -= (word_t) kernel_stack_alloc;
    cpu_id = sp >> CONFIG_KERNEL_STACK_BITS;
    return cpu_id;
}

static inline cpu_id_t cpuIndexToID(word_t index)
{
    return BIT(index);
}

#endif /* ENABLE_SMP_SUPPORT */
#endif /* __MODE_SMP_H_ */
