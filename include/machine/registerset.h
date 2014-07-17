/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MACHINE_REGISTERSET_H
#define __MACHINE_REGISTERSET_H

#include <arch/types.h>
#include <arch/machine/registerset.h>
#include <arch/object/structures.h>

static inline void
setRegister(tcb_t *thread, register_t reg, word_t w)
{
    thread->tcbContext.registers[reg] = w;
}

static inline word_t PURE
getRegister(tcb_t *thread, register_t reg)
{
    return thread->tcbContext.registers[reg];
}

#endif
