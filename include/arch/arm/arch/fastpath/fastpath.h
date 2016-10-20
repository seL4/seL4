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
#include <benchmark/benchmark_track.h>
#include <mode/machine/debug.h>

void slowpath(syscall_t syscall) NORETURN;

/** DONT_TRANSLATE */
static inline void NORETURN fastpath_restore(word_t badge, word_t msgInfo, tcb_t *cur_thread)
{

    c_exit_hook();

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(ksCurThread);
#endif

    register word_t badge_reg asm("r0") = badge;
    register word_t msgInfo_reg asm("r1") = msgInfo;
    register word_t cur_thread_reg asm("r2") = (word_t)cur_thread;

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        asm volatile( /* r0 and r1 should be preserved */
            "mov sp, r2         \n"
            /* Pop user registers, preserving r0 and r1 */
            "add sp, sp, #8     \n"
            "pop {r2-r12}       \n"
            /* Retore the user stack pointer */
            "pop {lr}           \n"
            "msr sp_usr, lr     \n"
            /* prepare the exception return lr */
            "ldr lr, [sp, #4]   \n"
            "msr elr_hyp, lr    \n"
            /* prepare the user status register */
            "ldr lr, [sp, #8]   \n"
            "msr spsr_hyp, lr   \n"
            /* Finally, pop our LR */
            "pop {lr}           \n"
            /* Return to user */
            "eret"
            :
            : [badge] "r" (badge_reg),
            [msginfo]"r"(msgInfo_reg),
            [cur_thread]"r"(cur_thread_reg)
            : "memory"
        );
    } else {
        asm volatile("mov sp, r2 \n\
                  add sp, sp, %[LR_SVC_OFFSET] \n\
                  ldmdb sp, {r2-lr}^ \n\
                  rfeia sp"
                     :
                     : [badge]"r"(badge_reg),
                     [msginfo]"r"(msgInfo_reg),
                     [cur_thread]"r"(cur_thread_reg),
                     [LR_SVC_OFFSET]"i"(LR_svc * sizeof(word_t))
                     : "memory"
                    );
    }
    UNREACHABLE();
}

void fastpath_call(word_t cptr, word_t r_msgInfo)
VISIBLE NORETURN SECTION(".vectors.fastpath_call");

void fastpath_reply_recv(word_t cptr, word_t r_msgInfo)
VISIBLE NORETURN SECTION(".vectors.fastpath_reply_recv");

#endif /* __ARCH_FASTPATH_H */

