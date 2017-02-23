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

#ifndef __MODE_SMP_H_
#define __MODE_SMP_H_

#include <config.h>


static inline CONST cpu_id_t
getCurrentCPUIndex(void)
{
#if CONFIG_MAX_NUM_NODES > 1
    cpu_id_t cpu_id;
    asm volatile ("mov %[cpu_id], mpidr_el1" : [cpu_id] "=r" (cpu_id) ::);
    return cpu_id;
#else
    return 0;
#endif
}

#endif /* __MODE_SMP_H_ */
