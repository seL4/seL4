/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_EXCEPTION_IPC
#define __LIBSEL4_ARCH_EXCEPTION_IPC

/**
 * NOT A STANDALONE INCLUDE
 */

/* Unknown syscall exception message. */
#define SEL4_EXCEPT_IPC_LABEL      3

/* User exception (such as divide by zero) message. */
#define SEL4_USER_EXCEPTION_LABEL  4
#define SEL4_USER_EXCEPTION_LENGTH 5

typedef enum {
    EXCEPT_IPC_USER_MR_FAULT_IP,
    EXCEPT_IPC_USER_MR_RSP,
    EXCEPT_IPC_USER_MR_RFLAGS,
    EXCEPT_IPC_USER_MR_NUMBER,
    EXCEPT_IPC_USER_MR_CODE,
    SEL4_USER_EXCEPT_IPC_LENGTH
} seL4_UserExceptIPCRegister;

typedef enum {
    EXCEPT_IPC_SYS_MR_RAX,
    EXCEPT_IPC_SYS_MR_RBX,
    EXCEPT_IPC_SYS_MR_RCX,
    EXCEPT_IPC_SYS_MR_RDX,
    EXCEPT_IPC_SYS_MR_RSI,
    EXCEPT_IPC_SYS_MR_RDI,
    EXCEPT_IPC_SYS_MR_RBP,
    EXCEPT_IPC_SYS_MR_R8,
    EXCEPT_IPC_SYS_MR_R9,
    EXCEPT_IPC_SYS_MR_R10,
    EXCEPT_IPC_SYS_MR_R11,
    EXCEPT_IPC_SYS_MR_R12,
    EXCEPT_IPC_SYS_MR_R13,
    EXCEPT_IPC_SYS_MR_R14,
    EXCEPT_IPC_SYS_MR_R15,
    EXCEPT_IPC_SYS_MR_RIP,
    EXCEPT_IPC_SYS_MR_RSP,
    EXCEPT_IPC_SYS_MR_RFLAGS,
    EXCEPT_IPC_SYS_MR_SYSCALL,
    SEL4_EXCEPT_IPC_LENGTH,
} seL4_ExceptIPCRegister;

typedef union {
    struct {
        seL4_Word fault_ip;
        seL4_Word rsp;
        seL4_Word rflags;
        seL4_Word exception_number;
        seL4_Word exception_code;
    } regs;
    seL4_Word raw[5];
} seL4_UserExceptionIpcRegisters;

typedef union {
    struct {
        seL4_Word rax;
        seL4_Word rbx;
        seL4_Word rcx;
        seL4_Word rdx;
        seL4_Word rsi;
        seL4_Word rdi;
        seL4_Word rbp;
        seL4_Word r8;
        seL4_Word r9;
        seL4_Word r10;
        seL4_Word r11;
        seL4_Word r12;
        seL4_Word r13;
        seL4_Word r14;
        seL4_Word r15;
        seL4_Word rip;
        seL4_Word rsp;
        seL4_Word rflags;
        seL4_Word syscall;
    } regs;
    seL4_Word raw[19];
} seL4_ExceptionIpcRegisters;

static inline seL4_Word seL4_isExceptIPC_Tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == SEL4_EXCEPT_IPC_LABEL;
}

static inline seL4_Word seL4_ExceptIPC_Get(seL4_Word mr)
{
    return seL4_GetMR(mr);
}

static inline void seL4_ExceptIPC_Set(seL4_Word index, seL4_Word val)
{
    seL4_SetMR(index, val);
}

static inline seL4_Word seL4_IsArchSyscallFrom(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_length(tag) == SEL4_EXCEPT_IPC_LENGTH;
}

static inline seL4_Word seL4_IsArchExceptionFrom(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_length(tag) == SEL4_USER_EXCEPTION_LENGTH;
}

#endif
