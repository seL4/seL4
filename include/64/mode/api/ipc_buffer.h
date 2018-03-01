/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __IPC_BUFFER_H
#define __IPC_BUFFER_H

#include <types.h>
#include <api/syscall.h>

static inline time_t mode_parseTimeArg(word_t i, word_t *buffer)
{
    return getSyscallArg(i, buffer);
}

static inline word_t mode_setTimeArg(word_t i, time_t time, word_t *buffer, tcb_t *thread)
{
    return setMR(thread, buffer, i, time);
}

#endif /* __IPC_BUFFER_H */
