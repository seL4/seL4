/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_SYSCALLS_H
#define __LIBSEL4_SEL4_ARCH_SYSCALLS_H

#include <autoconf.h>
#include <sel4/arch/functions.h>
#include <sel4/types.h>

/*
 * To simplify the definition of the various seL4 syscalls/syscall-wrappers we define
 * some helper assembly functions. These functions are designed to cover the different
 * cases of sending/receiving data in registers to/from the kernel. The most 'complex'
 * version is arm_sys_send_recv, and all other functions are limited versions that allow
 * for registers to not be unnecessarily clobbered
 *
 * arm_sys_send: Fills all registers into the kernel, expects nothing to be sent back
 *      by the kernel. Used for direction one way sends that contain data (e.g. seL4_Send,
 *      seL4_NBSend)
 *
 * arm_sys_send_null: Only fills metadata registers into the kernel (skips message
 *      registers). Expects nothing to be sent back by the kernel. Used by directional
 *      one way sends that do not contain data (e.g. seL4_Notify)
 *
 * arm_sys_reply: Similar to arm_sys_send except it does not take a word for the
 *      destination register. Used for undirected one way sends that contain data
 *      (e.g. seL4_Reply)
 *
 * arm_sys_recv: Sends one register (destination) to the kernel and expects all
 *      registers to be returned by the kernel. Used for directed receives that return
 *      data (e.g. seL4_Recv)
 *
 * arm_sys_send_recv: Fills all registers into the kernel and expects all of them
 *      to be filled on return by the kernel. Used for directed send+receives
 *      where data flows both directions (e.g. seL4_Call, seL4_ReplyWait)
 *
 * arm_sys_null: Does not send any registers to the kernel or expect anything to
 *      be returned from the kernel. Used to trigger implicit kernel actions without
 *      any data (e.g. seL4_Yield)
 */

static inline void
arm_sys_send(seL4_Word sys, seL4_Word dest, seL4_Word info_arg, seL4_Word mr0, seL4_Word mr1, seL4_Word mr2, seL4_Word mr3)
{
    register seL4_Word destptr asm("r0") = dest;
    register seL4_Word info asm("r1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = mr0;
    register seL4_Word msg1 asm("r3") = mr1;
    register seL4_Word msg2 asm("r4") = mr2;
    register seL4_Word msg3 asm("r5") = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (destptr), "+r" (msg0), "+r" (msg1), "+r" (msg2),
        "+r" (msg3), "+r" (info)
        : "r"(scno)
    );
}

static inline void arm_sys_reply(seL4_Word sys, seL4_Word info_arg, seL4_Word mr0, seL4_Word mr1, seL4_Word mr2, seL4_Word mr3)
{
    register seL4_Word info asm("r1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = mr0;
    register seL4_Word msg1 asm("r3") = mr1;
    register seL4_Word msg2 asm("r4") = mr2;
    register seL4_Word msg3 asm("r5") = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
        "+r" (info)
        : "r"(scno)
    );
}

static inline void
arm_sys_send_null(seL4_Word sys, seL4_Word src, seL4_Word info_arg)
{
    register seL4_Word destptr asm("r0") = src;
    register seL4_Word info asm("r1") = info_arg;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (destptr), "+r" (info)
        : "r"(scno)
    );
}

static inline void
arm_sys_recv(seL4_Word sys, seL4_Word src, seL4_Word *out_badge, seL4_Word *out_info, seL4_Word *out_mr0, seL4_Word *out_mr1, seL4_Word *out_mr2, seL4_Word *out_mr3)
{
    register seL4_Word src_and_badge asm("r0") = src;
    register seL4_Word info asm("r1");

    /* Incoming message registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "=r" (msg0), "=r" (msg1), "=r" (msg2), "=r" (msg3),
        "=r" (info), "+r" (src_and_badge)
        : "r"(scno)
        : "memory"
    );
    *out_badge = src_and_badge;
    *out_info = info;
    *out_mr0 = msg0;
    *out_mr1 = msg1;
    *out_mr2 = msg2;
    *out_mr3 = msg3;
}

static inline void
arm_sys_send_recv(seL4_Word sys, seL4_Word dest, seL4_Word *out_badge, seL4_Word info_arg, seL4_Word *out_info, seL4_Word *in_out_mr0, seL4_Word *in_out_mr1, seL4_Word *in_out_mr2, seL4_Word *in_out_mr3)
{
    register seL4_Word destptr asm("r0") = dest;
    register seL4_Word info asm("r1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = *in_out_mr0;
    register seL4_Word msg1 asm("r3") = *in_out_mr1;
    register seL4_Word msg2 asm("r4") = *in_out_mr2;
    register seL4_Word msg3 asm("r5") = *in_out_mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
        "+r" (info), "+r" (destptr)
        : "r"(scno)
        : "memory"
    );
    *out_info = info;
    *out_badge = destptr;
    *in_out_mr0 = msg0;
    *in_out_mr1 = msg1;
    *in_out_mr2 = msg2;
    *in_out_mr3 = msg3;
}

static inline void
arm_sys_null(seL4_Word sys)
{
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : /* no outputs */
        : "r"(scno)
    );
}

static inline void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    arm_sys_send(seL4_SysSend, dest, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1), seL4_GetMR(2), seL4_GetMR(3));
}

static inline void
seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    arm_sys_send(seL4_SysSend, dest, msgInfo.words[0],
                 mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr0 : 0,
                 mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr1 : 0,
                 mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr2 : 0,
                 mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr3 : 0
                );
}

static inline void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    arm_sys_send(seL4_SysNBSend, dest, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1), seL4_GetMR(2), seL4_GetMR(3));
}

static inline void
seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                   seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    arm_sys_send(seL4_SysNBSend, dest, msgInfo.words[0],
                 mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr0 : 0,
                 mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr1 : 0,
                 mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr2 : 0,
                 mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr3 : 0
                );
}

static inline void
seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    arm_sys_reply(seL4_SysReply, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1), seL4_GetMR(2), seL4_GetMR(3));
}

static inline void
seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                  seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    arm_sys_reply(seL4_SysReply, msgInfo.words[0],
                  mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr0 : 0,
                  mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr1 : 0,
                  mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr2 : 0,
                  mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr3 : 0
                 );
}

static inline void
seL4_Signal(seL4_CPtr dest)
{
    arm_sys_send_null(seL4_SysSend, dest, seL4_MessageInfo_new(0, 0, 0, 0).words[0]);
}

static inline seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0;
    seL4_Word msg1;
    seL4_Word msg2;
    seL4_Word msg3;

    arm_sys_recv(seL4_SysRecv, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3);

    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;
}

static inline seL4_MessageInfo_t
seL4_RecvWithMRs(seL4_CPtr src, seL4_Word* sender,
                 seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0;
    seL4_Word msg1;
    seL4_Word msg2;
    seL4_Word msg3;

    arm_sys_recv(seL4_SysRecv, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3);

    /* Write the message back out to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;
}

static inline seL4_MessageInfo_t
seL4_NBRecv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0;
    seL4_Word msg1;
    seL4_Word msg2;
    seL4_Word msg3;

    arm_sys_recv(seL4_SysNBRecv, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3);

    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;
}

static inline seL4_MessageInfo_t
seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    seL4_MessageInfo_t info;
    seL4_Word msg0 = seL4_GetMR(0);
    seL4_Word msg1 = seL4_GetMR(1);
    seL4_Word msg2 = seL4_GetMR(2);
    seL4_Word msg3 = seL4_GetMR(3);

    arm_sys_send_recv(seL4_SysCall, dest, &dest, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3);

    /* Write out the data back to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    return info;
}

static inline seL4_MessageInfo_t
seL4_CallWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    seL4_MessageInfo_t info;
    seL4_Word msg0;
    seL4_Word msg1;
    seL4_Word msg2;
    seL4_Word msg3;

    /* Load beginning of the message into registers. */
    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    arm_sys_send_recv(seL4_SysCall, dest, &dest, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3);

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0;
    seL4_Word msg1;
    seL4_Word msg2;
    seL4_Word msg3;

    /* Load beginning of the message into registers. */
    msg0 = seL4_GetMR(0);
    msg1 = seL4_GetMR(1);
    msg2 = seL4_GetMR(2);
    msg3 = seL4_GetMR(3);

    arm_sys_send_recv(seL4_SysReplyRecv, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3);

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecvWithMRs(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0;
    seL4_Word msg1;
    seL4_Word msg2;
    seL4_Word msg3;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    arm_sys_send_recv(seL4_SysReplyRecv, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3);

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline void
seL4_Yield(void)
{
    arm_sys_null(seL4_SysYield);
    asm volatile("" ::: "memory");
}

#ifdef CONFIG_DEBUG_BUILD
static inline void
seL4_DebugPutChar(char c)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;
    seL4_Word unused4 = 0;
    seL4_Word unused5 = 0;

    arm_sys_send_recv(seL4_SysDebugPutChar, c, &unused0, 0, &unused1, &unused2, &unused3, &unused4, &unused5);
}

static inline void
seL4_DebugHalt(void)
{
    arm_sys_null(seL4_SysDebugHalt);
}

static inline void
seL4_DebugSnapshot(void)
{
    arm_sys_null(seL4_SysDebugSnapshot);
    asm volatile("" ::: "memory");
}

static inline seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;
    seL4_Word unused4 = 0;

    arm_sys_send_recv(seL4_SysDebugCapIdentify, cap, &cap, 0, &unused0, &unused1, &unused2, &unused3, &unused4);
    return (seL4_Uint32)cap;
}

char *strcpy(char *, const char *);
static inline void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name)
{
    strcpy((char*)seL4_GetIPCBuffer()->msg, name);

    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;
    seL4_Word unused4 = 0;
    seL4_Word unused5 = 0;

    arm_sys_send_recv(seL4_SysDebugNameThread, tcb, &unused0, 0, &unused1, &unused2, &unused3, &unused4, &unused5);
}
#endif

#ifdef SEL4_DANGEROUS_CODE_INJECTION_KERNEL
static inline void
seL4_DebugRun(void (* userfn) (void *), void* userarg)
{
    arm_sys_send_null(seL4_SysDebugRun, (seL4_Word)userfn, (seL4_Word)userarg);
    asm volatile("" ::: "memory");
}
#endif

#ifdef CONFIG_ENABLE_BENCHMARKS
/* set the log index back to 0 */
static inline seL4_Error
seL4_BenchmarkResetLog(void)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;
    seL4_Word unused4 = 0;

    seL4_Word ret;
    arm_sys_send_recv(seL4_SysBenchmarkResetLog, 0, &ret, 0, &unused0, &unused1, &unused2, &unused3, &unused4);

    return (seL4_Error) ret;
}

static inline void
seL4_BenchmarkFinalizeLog(void)
{
    arm_sys_null(seL4_SysBenchmarkFinalizeLog);
    asm volatile("" ::: "memory");
}

static inline seL4_Error
seL4_BenchmarkSetLogBuffer(seL4_Word frame_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;
    seL4_Word unused4 = 0;

    arm_sys_send_recv(seL4_SysBenchmarkSetLogBuffer, frame_cptr, &frame_cptr, 0, &unused0, &unused1, &unused2, &unused3, &unused4);

    return (seL4_Error) frame_cptr;
}

static inline void
seL4_BenchmarkNullSyscall(void)
{
    arm_sys_null(seL4_SysBenchmarkNullSyscall);
    asm volatile("" ::: "memory");
}

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
static inline void
seL4_BenchmarkGetThreadUtilisation(seL4_Word tcp_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;
    seL4_Word unused4 = 0;
    seL4_Word unused5 = 0;

    arm_sys_send_recv(seL4_BenchmarkGetThreadUtilization, tcb_cptr, &unused0, 0, &unused1, &unused2, &unused3, &unused4, &unused5);
}

static inline void
seL4_BenchmarkResetThreadUtilisation(seL4_Word tcp_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;
    seL4_Word unused4 = 0;
    seL4_Word unused5 = 0;

    arm_sys_send_recv(seL4_BenchmarkResetThreadUtilization, tcb_cptr, &unused0, 0, &unused1, &unused2, &unused3, &unused4, &unused5);
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */

#endif
