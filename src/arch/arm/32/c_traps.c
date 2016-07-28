/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <model/statedata.h>
#include <arch/fastpath/fastpath.h>
#include <arch/kernel/traps.h>
#include <api/syscall.h>
#include <arch/linker.h>

#include <benchmark_track.h>
#include <benchmark_utilisation.h>

/** DONT_TRANSLATE */
static inline void FORCE_INLINE NORETURN restore_user_context(void)
{
    word_t cur_thread_reg = (word_t) ksCurThread;

    c_exit_hook();

    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        asm volatile(
            /* Set stack pointer to point at the r0 of the user context. */
            "mov sp, %[cur_thread_reg] \n"
            /* Pop user registers */
            "pop {r0-r12}              \n"
            /* Retore the user stack pointer */
            "pop {lr}                  \n"
            "msr sp_usr, lr            \n"
            /* prepare the eception return lr */
            "ldr lr, [sp, #4]          \n"
            "msr elr_hyp, lr           \n"
            /* prepare the user status register */
            "ldr lr, [sp, #8]          \n"
            "msr spsr_hyp, lr          \n"
            /* Finally, pop our LR */
            "pop {lr}                  \n"
            /* Return to user */
            "eret"
            : /* no output */
            : [cur_thread_reg] "r" (cur_thread_reg)
            : "memory"
        );
    } else {
        asm volatile("mov sp, %[cur_thread] \n\
                  ldmdb sp, {r0-lr}^ \n\
                  rfeia sp"
                     : /* no output */
                     : [cur_thread] "r" (cur_thread_reg + LR_svc * sizeof(word_t))
                    );
    }
    UNREACHABLE();
}

/** DONT_TRANSLATE */
void NORETURN slowpath(syscall_t syscall)
{
    handleSyscall(syscall);

    restore_user_context();
    UNREACHABLE();
}

/** DONT_TRANSLATE */
void VISIBLE c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
{
    c_entry_hook();
#if defined(DEBUG) || defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES)
    benchmark_debug_syscall_start(cptr, msgInfo, syscall);
#endif /* DEBUG */

#ifdef CONFIG_FASTPATH
    if (syscall == SysCall) {
        fastpath_call(cptr, msgInfo);
        UNREACHABLE();
    } else if (syscall == SysReplyRecv) {
        fastpath_reply_recv(cptr, msgInfo);
        UNREACHABLE();
    }
#endif /* CONFIG_FASTPATH */

    if (unlikely(syscall < SYSCALL_MIN || syscall > SYSCALL_MAX)) {
        handleUnknownSyscall(syscall);
        restore_user_context();
        UNREACHABLE();
    } else {
        slowpath(syscall);
        UNREACHABLE();
    }
}
