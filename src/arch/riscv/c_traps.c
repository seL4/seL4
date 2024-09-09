/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <model/statedata.h>
#include <arch/fastpath/fastpath.h>
#include <arch/kernel/traps.h>
#include <machine/debug.h>
#include <api/syscall.h>
#include <util.h>
#include <arch/machine/hardware.h>
#include <machine/fpu.h>

#include <benchmark/benchmark_track.h>
#include <benchmark/benchmark_utilisation.h>

/** DONT_TRANSLATE */
void VISIBLE NORETURN restore_user_context(void)
{
    pptr_t cur_thread_reg = (pptr_t) NODE_STATE(ksCurThread)->tcbArch.tcbContext.registers;
    c_exit_hook();
    NODE_UNLOCK_IF_HELD;

#ifdef ENABLE_SMP_SUPPORT
    register_t sp = read_sscratch();
#if defined(__CHERI_PURE_CAPABILITY__)
    sp -= sizeof(register_t);
    *((register_t *)sp) = cur_thread_reg;
#else
    sp -= sizeof(word_t);
    *((word_t *)sp) = cur_thread_reg;
#endif
#endif


#ifdef CONFIG_HAVE_FPU
    lazyFPURestore(NODE_STATE(ksCurThread));
    set_tcb_fs_state(NODE_STATE(ksCurThread), isFpuEnable());
#endif

    asm volatile(
#if defined(__CHERI_PURE_CAPABILITY__)
        "cmove ct0, %[cur_thread]   \n"
#else
        "mv t0, %[cur_thread]       \n"
#endif
        LOAD_S " "REGN(ra)  ", (0*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(sp)  ", (1*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(gp)  ", (2*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(tp)  ", (3*%[REGSIZE])("REGN(t0)")  \n"
        /* skip t0/x5, t1/x6, they are restored later */
        /* no-op store conditional to clear monitor state */
        /* this may succeed in implementations with very large reservations, but the saved ra is dead */
#if defined(__CHERI_PURE_CAPABILITY__)
        "csc.c   zero, c0, ("REGN(t0)")\n"
#else
        "sc.w zero, zero, ("REGN(t0)")\n"
#endif
        LOAD_S " "REGN(t2) ", (6*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(s0) ", (7*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(s1) ", (8*%[REGSIZE])("REGN(t0)")  \n"
        LOAD_S " "REGN(a0) ", (9*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a1) ", (10*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a2) ", (11*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a3) ", (12*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a4) ", (13*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a5) ", (14*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a6) ", (15*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(a7) ", (16*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s2) ", (17*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s3) ", (18*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s4) ", (19*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s5) ", (20*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s6) ", (21*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s7) ", (22*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s8) ", (23*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s9) ", (24*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(s10) ", (25*%[REGSIZE])("REGN(t0)")\n"
        LOAD_S " "REGN(s11) ", (26*%[REGSIZE])("REGN(t0)")\n"
        LOAD_S " "REGN(t3) ", (27*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t4) ", (28*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t5) ", (29*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t6) ", (30*%[REGSIZE])("REGN(t0)") \n"
        /* get sepc */
        LOAD_S " "REGN(t1) ", (34*%[REGSIZE])("REGN(t0)")\n"
#if defined(__CHERI_PURE_CAPABILITY__)
        "cspecialw sepcc, ct1  \n"
#else
        "csrw sepc, t1  \n"
#endif
#ifndef ENABLE_SMP_SUPPORT
        /* Write back sscratch with cur_thread_reg to get it back on the next trap entry */
#if defined(__CHERI_PURE_CAPABILITY__)
        "cspecialw sscratchc, ct0  \n"
#else
        "csrw sscratch, t0         \n"
#endif
#endif
        LOAD_S " "REGN(t1) ", (32*%[REGSIZE])("REGN(t0)") \n"
        "csrw sstatus, t1\n"

        LOAD_S " "REGN(t1) ", (5*%[REGSIZE])("REGN(t0)") \n"
        LOAD_S " "REGN(t0) ", (4*%[REGSIZE])("REGN(t0)") \n"
        "sret"
        : /* no output */
        : [REGSIZE] "i"(sizeof(register_t)),
        [cur_thread] ASM_REG_CONSTR(cur_thread_reg)
        : "memory"
    );

    UNREACHABLE();
}

void VISIBLE NORETURN c_handle_interrupt(void)
{
    NODE_LOCK_IRQ_IF(getActiveIRQ() != irq_remote_call_ipi);

    c_entry_hook();

    handleInterruptEntry();

    restore_user_context();
    UNREACHABLE();
}

void VISIBLE NORETURN c_handle_exception(void)
{
    NODE_LOCK_SYS;

    c_entry_hook();

    word_t scause = read_scause();
    switch (scause) {
    case RISCVInstructionAccessFault:
    case RISCVLoadAccessFault:
    case RISCVStoreAccessFault:
    case RISCVLoadPageFault:
    case RISCVStorePageFault:
    case RISCVInstructionPageFault:
    /* cheriTODO: We likely need to have a separate CHERI fault handling (and message IPC).
     * For now, let it be part of the VM memory faults.
     */
    case RISCVCheriFault:
        handleVMFaultEvent(scause);
        break;
    default:
#ifdef CONFIG_HAVE_FPU
        if (!isFpuEnable()) {
            /* we assume the illegal instruction is caused by FPU first */
            handleFPUFault();
            setNextPC(NODE_STATE(ksCurThread), getRestartPC(NODE_STATE(ksCurThread)));
            break;
        }
#endif
        handleUserLevelFault(scause, 0);
        break;
    }

    restore_user_context();
    UNREACHABLE();
}

void VISIBLE NORETURN slowpath(syscall_t syscall)
{
    if (unlikely(syscall < SYSCALL_MIN || syscall > SYSCALL_MAX)) {
#ifdef TRACK_KERNEL_ENTRIES
        ksKernelEntry.path = Entry_UnknownSyscall;
#endif /* TRACK_KERNEL_ENTRIES */
        /* Contrary to the name, this handles all non-standard syscalls used in
         * debug builds also.
         */
        handleUnknownSyscall(syscall);
    } else {
#ifdef TRACK_KERNEL_ENTRIES
        ksKernelEntry.is_fastpath = 0;
#endif /* TRACK KERNEL ENTRIES */
        handleSyscall(syscall);
    }

    restore_user_context();
    UNREACHABLE();
}

#ifdef CONFIG_FASTPATH
ALIGN(L1_CACHE_LINE_SIZE)
#ifdef CONFIG_KERNEL_MCS
void VISIBLE c_handle_fastpath_reply_recv(word_t cptr, word_t msgInfo, word_t reply)
#else
void VISIBLE c_handle_fastpath_reply_recv(word_t cptr, word_t msgInfo)
#endif
{
    NODE_LOCK_SYS;

    c_entry_hook();
#ifdef TRACK_KERNEL_ENTRIES
    benchmark_debug_syscall_start(cptr, msgInfo, SysReplyRecv);
    ksKernelEntry.is_fastpath = 1;
#endif /* DEBUG */
#ifdef CONFIG_KERNEL_MCS
    fastpath_reply_recv(cptr, msgInfo, reply);
#else
    fastpath_reply_recv(cptr, msgInfo);
#endif
    UNREACHABLE();
}

ALIGN(L1_CACHE_LINE_SIZE)
void VISIBLE c_handle_fastpath_call(word_t cptr, word_t msgInfo)
{
    NODE_LOCK_SYS;

    c_entry_hook();
#ifdef TRACK_KERNEL_ENTRIES
    benchmark_debug_syscall_start(cptr, msgInfo, SysCall);
    ksKernelEntry.is_fastpath = 1;
#endif /* DEBUG */

    fastpath_call(cptr, msgInfo);

    UNREACHABLE();
}
#endif

void VISIBLE NORETURN c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
{
    NODE_LOCK_SYS;

    c_entry_hook();
#ifdef TRACK_KERNEL_ENTRIES
    benchmark_debug_syscall_start(cptr, msgInfo, syscall);
    ksKernelEntry.is_fastpath = 0;
#endif /* DEBUG */
    slowpath(syscall);

    UNREACHABLE();
}
