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

#include <arch/linker.h>
#include <mode/fastpath/fastpath.h>
#include <plat/machine.h>

void slowpath(syscall_t syscall)
VISIBLE NORETURN;

void slowpath_irq(irq_t irq)
VISIBLE NORETURN;

void fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
VISIBLE NORETURN;

void fastpath_call(word_t cptr, word_t r_msgInfo)
VISIBLE NORETURN SECTION(".vectors.fastpath_call");

void fastpath_reply_recv(word_t cptr, word_t r_msgInfo)
VISIBLE NORETURN SECTION(".vectors.fastpath_reply_recv");

void fastpath_signal(word_t cptr)
VISIBLE NORETURN SECTION(".vectors.fastpath_signal");

void fastpath_irq(void)
VISIBLE SECTION(".vectors.fastpath_irq");


#endif /* __ARCH_FASTPATH_H */

