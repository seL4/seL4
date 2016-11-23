/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_FASTPATH_H
#define __ARCH_FASTPATH_H

#include <mode/fastpath/fastpath.h>

static inline int
fastpath_reply_cap_check(cap_t cap)
{
    return cap_capType_equals(cap, cap_reply_cap);
}

void slowpath(syscall_t syscall)
NORETURN;

void fastpath_call(word_t cptr, word_t r_msgInfo)
NORETURN;

void fastpath_reply_recv(word_t cptr, word_t r_msgInfo)
NORETURN;

#endif
