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

static inline parseTime_ret_t
arch_parseTimeArg(word_t i, word_t *buffer)
{
    return (parseTime_ret_t) {
        .words = 2,
         .arg = (((time_t) getSyscallArg(1, buffer)) << 32llu) + getSyscallArg(0, buffer),
    };
}


#endif /* __ARCH_IPC_BUFFER_H */

