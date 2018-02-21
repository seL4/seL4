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
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __ARCH_MODEL_SMP_H_
#define __ARCH_MODEL_SMP_H_

#include <config.h>
#include <mode/smp/smp.h>
#include <model/smp.h>

#ifdef ENABLE_SMP_SUPPORT
static inline bool_t
try_arch_atomic_exchange(void *ptr, void *new_val, void **prev, int success_memorder, int
                         failure_memorder)
{
    *prev = __atomic_exchange_n((void **) ptr, new_val, success_memorder);
    return true;
}

#endif /* ENABLE_SMP_SUPPORT */
#endif /* __ARCH_MODEL_SMP_H_ */
