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

#ifndef __ARCH_KERNEL_IPI_INLINE_H
#define __ARCH_KERNEL_IPI_INLINE_H

#include <config.h>
#include <smp/ipi.h>

#if CONFIG_MAX_NUM_NODES > 1
static inline void doRemoteStall(word_t cpu)
{
    /* TODO */
#warning "doRemoteStall is not implemented for ARM yet"
}

#endif /* CONFIG_MAX_NUM_NODES > 1 */
#endif /* __ARCH_KERNEL_IPI_INLINE_H */
