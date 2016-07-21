/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __IPC_BUFFER_H
#define __IPC_BUFFER_H

#include <types.h>
#include <api/syscall.h>
#include <object/tcb.h>

#define TIME_ARG_SIZE (sizeof(time_t) / sizeof(word_t))

static inline time_t
mode_parseTimeArg(word_t i, word_t *buffer)
{
    return ((time_t) getSyscallArg(i + 1, buffer) << 32llu) + getSyscallArg(i + 0, buffer);
}

static inline word_t
mode_setTimeArg(word_t i, time_t arg, word_t *buffer, tcb_t *thread)
{
    setMR(thread, buffer, i, (uint32_t) (arg >> 32llu));
    return setMR(thread, buffer, i + 1u, (uint32_t) (arg));
}

#endif /* __IPC_BUFFER_H */

