/*
 * Copyright 2016, Data61 CSIRO
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#include <config.h>
#include <arch/kernel/traps.h>
#include <api/syscall.h>

#include <benchmark_track.h>
#include <benchmark_utilisation.h>

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
