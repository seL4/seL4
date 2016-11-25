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

#include <util.h>
#include <arch/types.h>
#include <arch/machine/registerset.h>
#include <arch/object/structures.h>

typedef enum {
    MessageID_Syscall,
    MessageID_Exception
} MessageID_t;

#define MAX_MSG_SIZE MAX(n_syscallMessage, n_exceptionMessage)
extern const register_t fault_messages[][MAX_MSG_SIZE] VISIBLE;

static inline void
setRegister(tcb_t *thread, register_t reg, word_t w)
{
    thread->tcbArch.tcbContext.registers[reg] = w;
}

static inline word_t PURE
getRegister(tcb_t *thread, register_t reg)
{
    return thread->tcbArch.tcbContext.registers[reg];
}

#endif
