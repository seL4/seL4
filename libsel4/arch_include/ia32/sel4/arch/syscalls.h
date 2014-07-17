/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_SYSCALLS_H
#define __LIBSEL4_ARCH_SYSCALLS_H

#include <autoconf.h>
#include <sel4/types.h>

static inline void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : "%edx"
    );
}

static inline void
seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0 != NULL ? *mr0 : 0),
        "c" (mr1 != NULL ? *mr1 : 0)
        : "%edx"
    );
}

static inline void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysNBSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : "%edx"
    );
}

static inline void
seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                   seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysNBSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0 != NULL ? *mr0 : 0),
        "c" (mr1 != NULL ? *mr1 : 0)
        : "%edx"
    );
}

static inline void
seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysReply),
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : "%ebx", "%edx"
    );
}

static inline void
seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                  seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysReply),
        "S" (msgInfo.words[0]),
        "D" (mr0 != NULL ? *mr0 : 0),
        "c" (mr1 != NULL ? *mr1 : 0)
        : "%ebx", "%edx"
    );
}

static inline void
seL4_Notify(seL4_CPtr dest, seL4_Word msg)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysSend),
        "b" (dest),
        "S" (seL4_MessageInfo_new(0, 0, 0, 1).words[0]),
        "D" (msg)
        : "%ecx", "%edx"
    );
}

static inline seL4_MessageInfo_t
seL4_Wait(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0;
    seL4_Word mr1;

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysWait),
        "b" (src)
        : "%edx", "memory"
    );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_WaitWithMRs(seL4_CPtr src, seL4_Word* sender,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1)
        : "a" (seL4_SysWait),
        "b" (src)
        : "%edx", "memory"
    );

    if (mr0 != NULL) {
        *mr0 = msg0;
    }
    if (mr1 != NULL) {
        *mr1 = msg1;
    }

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

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1),
        "=b" (dest) /* dummy, tells GCC that ebx is clobbered */
        : "a" (seL4_SysCall),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0),
        "c" (mr1)
        : "%edx", "memory"
    );

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

    if (mr0 != NULL && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != NULL && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1),
        "=b" (dest) /* dummy, tells GCC that ebx is clobbered */
        : "a" (seL4_SysCall),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (msg0),
        "c" (msg1)
        : "%edx", "memory"
    );

    if (mr0 != NULL) {
        *mr0 = msg0;
    }
    if (mr1 != NULL) {
        *mr1 = msg1;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyWait(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0 = seL4_GetMR(0);
    seL4_Word mr1 = seL4_GetMR(1);

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysReplyWait),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0),
        "c" (mr1)
        : "%edx", "memory"
    );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyWaitWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    if (mr0 != NULL && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != NULL && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1)
        : "a" (seL4_SysReplyWait),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (msg0),
        "c" (msg1)
        : "%edx", "memory"
    );

    if (mr0 != NULL) {
        *mr0 = msg0;
    }
    if (mr1 != NULL) {
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
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysYield)
        : "%ebx", "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugPutChar(char c)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugPutChar),
        "b" (c)
        : "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline void
seL4_DebugHalt(void)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugHalt)
        : "%ebx", "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugSnapshot(void)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugSnapshot)
        : "%ebx", "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline uint32_t
seL4_DebugCapIdentify(seL4_CPtr cap)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        : "=b"(cap)
        : "a"(seL4_SysDebugCapIdentify), "b"(cap)
        : "%ecx", "%edx", "%esi", "%edi", "memory"
    );
    return (uint32_t)cap;
}
#endif

#if defined(SEL4_DANGEROUS_CODE_INJECTION_KERNEL)
static inline void
seL4_DebugRun(void (*userfn) (void *), void* userarg)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugRun),
        "b" (userfn),
        "S" (userarg)
        : "%ecx", "%edx", "%edi", "memory"
    );
}
#endif

#ifdef CONFIG_BENCHMARK
static inline void
seL4_BenchmarkResetLog(void)
{
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        :
        : "a" (seL4_SysBenchmarkResetLog)
        : "%ecx", "%edx", "%edi", "memory"
    );
}

static inline uint32_t
seL4_BenchmarkDumpLog(seL4_Word start, seL4_Word size)
{
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        : "=b" (start)
        : "a" (seL4_SysBenchmarkDumpLog),
        "b" (start),
        "S" (size)
        : "%ecx", "%edx", "%edi", "memory"
    );

    return (uint32_t) start;
}


static inline uint32_t
seL4_BenchmarkLogSize(void)
{
    uint32_t ret = 0;
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        : "=b" (ret)
        : "a" (seL4_SysBenchmarkLogSize)
        : "%ecx", "%edx", "%edi", "memory"
    );

    return ret;
}

#endif /* CONFIG_BENCHMARK */
#endif
