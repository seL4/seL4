/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_IPC_BUFFER_H
#define __ARCH_IPC_BUFFER_H

#include <types.h>
#include <api/syscall.h>

static inline time_t
arch_parseTimeArg(word_t i, word_t *buffer)
{
    return ((time_t) getSyscallArg(i + 1, buffer) << 32llu) + getSyscallArg(i + 0, buffer);
}

static inline void
arch_setTimeArg(word_t i, time_t arg)
{
    setRegister(ksCurThread, msgRegisters[i], (uint32_t) arg);
    setRegister(ksCurThread, msgRegisters[i + 1], (uint32_t) (arg << 32llu));
}

#endif /* __ARCH_IPC_BUFFER_H */

