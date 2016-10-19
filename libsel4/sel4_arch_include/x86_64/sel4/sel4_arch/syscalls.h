/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(D61_BSD)
 */

#ifndef __LIBSEL4_SEL4_SEL4_ARCH_SYSCALLS_H_
#define __LIBSEL4_SEL4_SEL4_ARCH_SYSCALLS_H_

#include <autoconf.h>

#if defined(CONFIG_SYSENTER)
#include <sel4/sel4_arch/syscalls_sysenter.h>
#elif defined(CONFIG_SYSCALL)
#include <sel4/sel4_arch/syscalls_syscall.h>
#else
#error Unknown method for kernel syscalls
#endif

static inline void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    x64_sys_send(seL4_SysSend, dest, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1));
}

static inline void
seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    x64_sys_send(seL4_SysSend, dest, msgInfo.words[0],
                 (mr0 != seL4_Null) ? *mr0 : 0,
                 (mr1 != seL4_Null) ? *mr1 : 0
                );
}

static inline void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    x64_sys_send(seL4_SysNBSend, dest, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1));
}

static inline void
seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                   seL4_Word *mr0, seL4_Word *mr1)
{
    x64_sys_send(seL4_SysNBSend, dest, msgInfo.words[0],
                 (mr0 != seL4_Null) ? *mr0 : 0,
                 (mr1 != seL4_Null) ? *mr1 : 0
                );
}

static inline void
seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    x64_sys_reply(seL4_SysReply, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1));
}

static inline void
seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                  seL4_Word *mr0, seL4_Word *mr1)
{
    x64_sys_reply(seL4_SysReply, msgInfo.words[0],
                  (mr0 != seL4_Null) ? *mr0 : 0,
                  (mr1 != seL4_Null) ? *mr1 : 0
                 );
}

static inline void
seL4_Signal(seL4_CPtr dest)
{
    x64_sys_send_null(seL4_SysSend, dest, seL4_MessageInfo_new(0, 0, 0, 1).words[0]);
}

static inline seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0;
    seL4_Word mr1;

    x64_sys_recv(seL4_SysRecv, src, &badge, &info.words[0], &mr0, &mr1);

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_RecvWithMRs(seL4_CPtr src, seL4_Word* sender,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0;
    seL4_Word msg1;

    x64_sys_recv(seL4_SysRecv, src, &badge, &info.words[0], &msg0, &msg1);

    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }

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
    seL4_Word mr0;
    seL4_Word mr1;

    x64_sys_recv(seL4_SysNBRecv, src, &badge, &info.words[0], &mr0, &mr1);

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{

    seL4_MessageInfo_t info;
    seL4_Word mr0 = seL4_GetMR(0);
    seL4_Word mr1 = seL4_GetMR(1);

    x64_sys_send_recv(seL4_SysCall, dest, &dest, msgInfo.words[0], &info.words[0], &mr0, &mr1);

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    return info;
}

static inline seL4_MessageInfo_t
seL4_CallWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }

    x64_sys_send_recv(seL4_SysCall, dest, &dest, msgInfo.words[0], &info.words[0], &msg0, &msg1);

    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0 = seL4_GetMR(0);
    seL4_Word mr1 = seL4_GetMR(1);

    x64_sys_send_recv(seL4_SysReplyRecv, dest, &badge, msgInfo.words[0], &info.words[0], &mr0, &mr1);

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecvWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }

    x64_sys_send_recv(seL4_SysReplyRecv, dest, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1);

    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline void
seL4_Yield(void)
{
    x64_sys_null(seL4_SysYield);
    asm volatile("" ::: "memory");
}

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugPutChar(char c)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;

    x64_sys_send_recv(seL4_SysDebugPutChar, c, &unused0, 0, &unused1, &unused2, &unused3);
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline void
seL4_DebugHalt(void)
{
    x64_sys_null(seL4_SysDebugHalt);
    asm volatile("" :::"memory");
}
#endif

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugSnapshot(void)
{
    x64_sys_null(seL4_SysDebugSnapshot);
    asm volatile("" :::"memory");
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;

    x64_sys_send_recv(seL4_SysDebugCapIdentify, cap, &cap, 0, &unused0, &unused1, &unused2);
    return (seL4_Uint32)cap;
}
#endif

#ifdef SEL4_DEBUG_KERNEL
char *strcpy(char *, const char *);
static inline void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name)
{

    strcpy((char*)seL4_GetIPCBuffer()->msg, name);

    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;

    x64_sys_send_recv(seL4_SysDebugNameThread, tcb, &unused0, 0, &unused1, &unused2, &unused3);
}
#endif

#if defined(SEL4_DANGEROUS_CODE_INJECTION_KERNEL)
static inline void
seL4_DebugRun(void (*userfn) (void *), void* userarg)
{
    x64_sys_send_null(seL4_SysDebugRun, (seL4_Word)userfn, (seL4_Word)userarg);
    asm volatile("" ::: "memory");
}
#endif

#if CONFIG_ENABLE_BENCHMARKS
static inline seL4_Error
seL4_BenchmarkResetLog(void)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;

    seL4_Word ret;

    x64_sys_send_recv(seL4_SysBenchmarkResetLog, 0, &ret, 0, &unused0, &unused1, &unused2);

    return (seL4_Error)ret;
}

static inline void
seL4_BenchmarkFinalizeLog(void)
{
    x64_sys_null(seL4_SysBenchmarkFinalizeLog);
    asm volatile("" ::: "memory");
}

static inline seL4_Error
seL4_BenchmarkSetLogBuffer(seL4_Word frame_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;

    x64_sys_send_recv(seL4_SysBenchmarkSetLogBuffer, frame_cptr, &frame_cptr, 0, &unused0, &unused1, &unused2);

    return (seL4_Error) frame_cptr;
}

static inline void
seL4_BenchmarkNullSyscall(void)
{
    x64_sys_null(seL4_SysBenchmarkNullSyscall);
    asm volatile("" ::: "memory");
}

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
static inline void
seL4_BenchmarkGetThreadUtilisation(seL4_Word tcb_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;

    x64_sys_send_recv(seL4_SysBenchmarkGetThreadUtilisation, tcb_cptr, &unused0, 0, &unused1, &unused2, &unused3);
}

static inline void
seL4_BenchmarkResetThreadUtilisation(seL4_Word tcb_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Word unused2 = 0;
    seL4_Word unused3 = 0;

    x64_sys_send_recv(seL4_SysBenchmarkResetThreadUtilisation, tcb_cptr, &unused0, 0, &unused1, &unused2, &unused3);
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */

#endif /* __LIBSEL4_SEL4_SEL4_ARCH_SYSCALLS_H_ */
