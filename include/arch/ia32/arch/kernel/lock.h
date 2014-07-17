/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_LOCK_H
#define __ARCH_KERNEL_LOCK_H

#ifdef DEBUG

#include <types.h>

typedef uint32_t lock_t;

/* global spinlocks */

extern lock_t lock_debug;

/* acquire/release lock */

static inline void lock_acquire(lock_t* lock)
{
    asm volatile (
        "1:                 \n"
        "movl  $1,    %%eax \n"
        "xchgl (%0),  %%eax \n"
        "testl %%eax, %%eax \n"
        "jnz   1b           \n"
        :
        : "r"(lock)
        : "%eax", "cc", "memory"
    );
}

static inline void lock_release(lock_t* lock)
{
    *lock = 0;
}
#endif /* DEBUG */

#endif
