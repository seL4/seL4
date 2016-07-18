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

static inline void FORCE_INLINE NORETURN restore_user_context(void)
{
    word_t cur_thread_reg = (word_t) ksCurThread;
    asm volatile("mov sp, %[cur_thread] \n\
                  ldmdb sp, {r0-lr}^ \n\
                  rfeia sp"
                 : /* no output */
                 : [cur_thread] "r" (cur_thread_reg + LR_svc * sizeof(word_t))
                 : "memory");
    UNREACHABLE();
}

void NORETURN VISIBLE slowpath(syscall_t syscall)
{
    handleSyscall(syscall);

    restore_user_context();
    UNREACHABLE();
}

/** DONT_TRANSLATE */
void VISIBLE c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
{
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
