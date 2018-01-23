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

#ifndef __ARCH_MODEL_SMP_H_
#define __ARCH_MODEL_SMP_H_

#include <config.h>
#include <mode/smp/smp.h>
#include <model/smp.h>

#ifdef ENABLE_SMP_SUPPORT
static inline cpu_id_t cpuIndexToID(word_t index)
{
    return BIT(index);
}

static inline bool_t
try_arch_atomic_exchange(void* ptr, void *new_val, void **prev, int success_memorder, int failure_memorder)
{
    uint32_t atomic_status;
    void *temp;

    asm volatile (
        LD_EX "%[prev_output], [%[ptr_val]]             \n\t" /* ret = *ptr */
        ST_EX "%" OP_WIDTH "[atomic_var], %[new_val] , [%[ptr_val]] \n\t"  /* *ptr = new */
        : [atomic_var] "=&r"(atomic_status), [prev_output]"=&r"(temp)     /* output */
        : [ptr_val] "r"(ptr), [new_val] "r" (new_val)  /* input */
        :
    );

    *prev = temp;

    /* Atomic operation success */
    if (likely(!atomic_status)) {
        __atomic_thread_fence(success_memorder);
    } else {
        /* Atomic operation failure */
        __atomic_thread_fence(failure_memorder);
    }

    /* On ARM if an atomic operation succeeds, it returns 0 */
    return (atomic_status == 0);
}

#endif /* ENABLE_SMP_SUPPORT */

#endif /* __ARCH_MODEL_SMP_H_ */
