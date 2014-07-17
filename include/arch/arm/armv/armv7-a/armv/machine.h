/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_ARMV7A_MACHINE_H
#define __ARCH_ARMV7A_MACHINE_H

/** MODIFIES: [*] */
static inline void dsb(void)
{
    asm volatile("dsb" ::: "memory");
}

/** MODIFIES: [*] */
static inline void dmb(void)
{
    asm volatile("dmb" ::: "memory");
}

/** MODIFIES: [*] */
static inline void isb(void)
{
    asm volatile("isb" ::: "memory");
}

#endif
