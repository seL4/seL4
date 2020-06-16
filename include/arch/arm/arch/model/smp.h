/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <mode/smp/smp.h>
#include <model/smp.h>

#ifdef ENABLE_SMP_SUPPORT
static inline cpu_id_t cpuIndexToID(word_t index)
{
    return BIT(index);
}

static inline bool_t try_arch_atomic_exchange_rlx(void *ptr, void *new_val, void **prev)
{
    uint32_t atomic_status;
    void *temp;

    asm volatile(
        LD_EX "%[prev_output], [%[ptr_val]]             \n\t" /* ret = *ptr */
        ST_EX "%" OP_WIDTH "[atomic_var], %[new_val] , [%[ptr_val]] \n\t"  /* *ptr = new */
        : [atomic_var] "=&r"(atomic_status), [prev_output]"=&r"(temp)     /* output */
        : [ptr_val] "r"(ptr), [new_val] "r"(new_val)   /* input */
        :
    );

    *prev = temp;

    /* On ARM if an atomic operation succeeds, it returns 0 */
    return (atomic_status == 0);
}

#endif /* ENABLE_SMP_SUPPORT */

