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

#if defined(__pic__)

#define SEL4_REGS_SAVE    "pushl %%ebx       \n"
#define SEL4_REGS_MOV     "movl %%edx, %%ebx \n"
#define SEL4_REGS_RESTORE "popl %%ebx        \n"
#define SEL4_REGS_RESTORE_EDX  \
        "movl %%ebx, %%edx \n" \
        "popl %%ebx        \n"

#define SEL4_REGS_OUT     "+d" (dest)
#define SEL4_REGS_OUT_IN(VAR) "+d" (VAR)
#define SEL4_REGS_IN
#define SEL4_REGS_OUT_VAR(VAR) \
        "=d" (VAR),
#define SEL4_REGS_IN_VAR(VAR) \
        "d" (VAR)

#define SEL4_REGS_CLOBBER_EBX
#define SEL4_REGS_CLOBBER_EDX
#define SEL4_REGS_CLOBBER_COMMA_EDX

#else

#define SEL4_REGS_SAVE
#define SEL4_REGS_MOV
#define SEL4_REGS_RESTORE
#define SEL4_REGS_RESTORE_EDX

#define SEL4_REGS_OUT
#define SEL4_REGS_OUT_IN(VAR) "+b" (VAR)
#define SEL4_REGS_IN "b" (dest),
#define SEL4_REGS_OUT_VAR(VAR) \
        "=b" (VAR),
#define SEL4_REGS_IN_VAR(VAR) \
        "b" (VAR)

#define SEL4_REGS_CLOBBER_EBX "%ebx",
#define SEL4_REGS_CLOBBER_EDX "%edx"
#define SEL4_REGS_CLOBBER_COMMA_EDX , "%edx"

#endif /* #if defined(__pic__) */

static inline void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT
        : "a" (seL4_SysSend),
        SEL4_REGS_IN
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : SEL4_REGS_CLOBBER_EDX
    );
}

static inline void
seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT
        : "a" (seL4_SysSend),
        SEL4_REGS_IN
        "S" (msgInfo.words[0]),
        "D" (mr0 != seL4_Null ? *mr0 : 0),
        "c" (mr1 != seL4_Null ? *mr1 : 0)
        : SEL4_REGS_CLOBBER_EDX
    );
}

static inline void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT
        : "a" (seL4_SysNBSend),
        SEL4_REGS_IN
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : SEL4_REGS_CLOBBER_EDX
    );
}

static inline void
seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                   seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT
        : "a" (seL4_SysNBSend),
        SEL4_REGS_IN
        "S" (msgInfo.words[0]),
        "D" (mr0 != seL4_Null ? *mr0 : 0),
        "c" (mr1 != seL4_Null ? *mr1 : 0)
        : SEL4_REGS_CLOBBER_EDX
    );
}

static inline void
seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysReply),
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : SEL4_REGS_CLOBBER_EBX "%edx"
    );
}

static inline void
seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                  seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysReply),
        "S" (msgInfo.words[0]),
        "D" (mr0 != seL4_Null ? *mr0 : 0),
        "c" (mr1 != seL4_Null ? *mr1 : 0)
        : SEL4_REGS_CLOBBER_EBX "%edx"
    );
}

static inline void
seL4_Signal(seL4_CPtr dest)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT
        : "a" (seL4_SysSend),
        SEL4_REGS_IN
        "S" (seL4_MessageInfo_new(0, 0, 0, 0).words[0])
        : "%ecx" SEL4_REGS_CLOBBER_COMMA_EDX
    );
}

static inline seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0;
    seL4_Word mr1;

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        SEL4_REGS_OUT_VAR(badge)
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysRecv),
        SEL4_REGS_IN_VAR(src)
        : "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

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
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        SEL4_REGS_OUT_VAR(badge)
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1)
        : "a" (seL4_SysRecv),
        SEL4_REGS_IN_VAR(src)
        : "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

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

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        SEL4_REGS_OUT_VAR(badge)
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysNBRecv),
        SEL4_REGS_IN_VAR(src)
        : "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

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

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1),
        SEL4_REGS_OUT_IN(dest)
        : "a" (seL4_SysCall),
        "S" (msgInfo.words[0]),
        "D" (mr0),
        "c" (mr1)
        : "memory" SEL4_REGS_CLOBBER_COMMA_EDX
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

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1),
        SEL4_REGS_OUT_IN(dest)
        : "a" (seL4_SysCall),
        "S" (msgInfo.words[0]),
        "D" (msg0),
        "c" (msg1)
        : "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

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

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        SEL4_REGS_OUT_VAR(badge)
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysReplyRecv),
        SEL4_REGS_IN_VAR(dest),
        "S" (msgInfo.words[0]),
        "D" (mr0),
        "c" (mr1)
        : "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

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

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        SEL4_REGS_OUT_VAR(badge)
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1)
        : "a" (seL4_SysReplyRecv),
        SEL4_REGS_IN_VAR(dest),
        "S" (msgInfo.words[0]),
        "D" (msg0),
        "c" (msg1)
        : "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

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
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysYield)
        : SEL4_REGS_CLOBBER_EBX "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}

#if defined(CONFIG_DEBUG_BUILD)
static inline void
seL4_DebugPutChar(char c)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT_IN(c)
        : "a" (seL4_SysDebugPutChar)
        : "%ecx", "%esi", "%edi", "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );
}
#endif

#ifdef CONFIG_DEBUG_BUILD
static inline void
seL4_DebugHalt(void)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugHalt)
        : SEL4_REGS_CLOBBER_EBX "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#if defined(CONFIG_DEBUG_BUILD)
static inline void
seL4_DebugSnapshot(void)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugSnapshot)
        : SEL4_REGS_CLOBBER_EBX "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#ifdef CONFIG_DEBUG_BUILD
static inline seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE_EDX
        "popl %%ebp        \n"
        : SEL4_REGS_OUT_IN(cap)
        : "a"(seL4_SysDebugCapIdentify)
        : "%ecx", "%esi", "%edi", "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );
    return (seL4_Uint32)cap;
}

char *strcpy(char *, const char *);
static inline void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name)
{
    strcpy((char*)seL4_GetIPCBuffer()->msg, name);

    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT_IN(tcb)
        : "a"(seL4_SysDebugNameThread)
        : "%ecx", "%esi", "%edi", "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );
}
#endif

#if defined(SEL4_DANGEROUS_CODE_INJECTION_KERNEL)
static inline void
seL4_DebugRun(void (*userfn) (void *), void* userarg)
{
    asm volatile (
        "pushl %%ebp       \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx \n"
        "movl %%edx, %%ebx \n"
        SEL4_REGS_MOV
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        SEL4_REGS_RESTORE
        "popl %%ebp        \n"
        : SEL4_REGS_OUT_IN(userfn)
        : "a" (seL4_SysDebugRun),
        "S" (userarg)
        : SEL4_REGS_CLOBBER_EBX "%ecx", "edx", "%edi", "memory"
    );
}
#endif

#ifdef CONFIG_ENABLE_BENCHMARKS
static inline seL4_Error
seL4_BenchmarkResetLog(void)
{
    seL4_Error ret = seL4_NoError;
    asm volatile (
        "pushl %%ebp        \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        SEL4_REGS_RESTORE_EDX
        "popl %%ebp         \n"
        : SEL4_REGS_OUT_VAR(ret)
        : "a" (seL4_SysBenchmarkResetLog)
        : "%ecx", "%edi", "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

    return ret;
}

static inline void
seL4_BenchmarkFinalizeLog(void)
{
    asm volatile (
        "pushl %%ebp        \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        SEL4_REGS_RESTORE
        "popl %%ebp         \n"
        :
        : "a" (seL4_SysBenchmarkFinalizeLog)
        : "%ecx", "%edi", "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );
}

static inline seL4_Error
seL4_BenchmarkSetLogBuffer(seL4_Word frame_cptr)
{
    asm volatile (
        "pushl %%ebp        \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx  \n"
        SEL4_REGS_MOVE
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        SEL4_REGS_RESTORE_EDX
        "popl %%ebp         \n"
        : SEL4_REGS_OUT_IN(frame_cptr)
        : "a" (seL4_SysBenchmarkSetLogBuffer)
        : "%ecx", "%edi", "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );

    return (seL4_Error) frame_cptr;
}

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
static inline void
seL4_BenchmarkGetThreadUtilisation(seL4_Word tcp_cptr)
{
    asm volatile (
        "pushl %%ebp        \n"
        SEL4_REGS_SAVE
        "movl %%esp, %%ecx  \n"
        SEL4_REGS_MOVE
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        SEL4_REGS_RESTORE
        "popl %%ebp         \n"
        :
        : "a" (seL4_SysBenchmarkGetThreadUtilisation),
          SEL4_REGS_IN_VAR(tcb_cptr)
        : "%ecx", "%edi", "memory" SEL4_REGS_CLOBBER_COMMA_EDX
    );
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */

#undef SEL4_REGS_SAVE
#undef SEL4_REGS_MOV
#undef SEL4_REGS_RESTORE
#undef SEL4_REGS_RESTORE_EDX

#undef SEL4_REGS_OUT
#undef SEL4_REGS_OUT_IN
#undef SEL4_REGS_IN
#undef SEL4_REGS_OUT_VAR
#undef SEL4_REGS_IN_VAR

#undef SEL4_REGS_CLOBBER_EBX
#undef SEL4_REGS_CLOBBER_EDX
#undef SEL4_REGS_CLOBBER_COMMA_EDX

#endif
