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

/** DONT_TRANSLATE */
static inline void NORETURN fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{
    register word_t badge_reg asm("r0") = badge;
    register word_t msgInfo_reg asm("r1") = msgInfo;
    register word_t cur_thread_reg asm("r2") = (word_t)cur_thread;
    asm volatile("mov sp, r2 \n\
                  add sp, sp, %[LR_SVC_OFFSET] \n\
                  ldmdb sp, {r2-lr}^ \n\
                  rfeia sp"
                 :
                 : [badge]"r"(badge_reg),
                 [msginfo]"r"(msgInfo_reg),
                 [cur_thread]"r"(cur_thread_reg),
                 [LR_SVC_OFFSET]"i"(LR_svc * sizeof(word_t))
                 : "memory");
    UNREACHABLE();
}

void fastpath_call(word_t cptr, word_t r_msgInfo)
VISIBLE NORETURN SECTION(".vectors.fastpath_call");

void fastpath_reply_recv(word_t cptr, word_t r_msgInfo)
VISIBLE NORETURN SECTION(".vectors.fastpath_reply_recv");

void fastpath_signal(word_t cptr)
VISIBLE NORETURN SECTION(".vectors.fastpath_signal");

void fastpath_irq(void)
VISIBLE SECTION(".vectors.fastpath_irq");


#endif /* __ARCH_FASTPATH_H */

