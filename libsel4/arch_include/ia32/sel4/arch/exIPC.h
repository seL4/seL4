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
    EXCEPT_IPC_USER_MR_ESP,
    EXCEPT_IPC_USER_MR_EFLAGS,
    EXCEPT_IPC_USER_MR_NUMBER,
    EXCEPT_IPC_USER_MR_CODE,
    SEL4_USER_EXCEPT_IPC_LENGTH
} seL4_UserExceptIPCRegister;

typedef enum {
    EXCEPT_IPC_SYS_MR_EAX,
    EXCEPT_IPC_SYS_MR_EBX,
    EXCEPT_IPC_SYS_MR_ECX,
    EXCEPT_IPC_SYS_MR_EDX,
    EXCEPT_IPC_SYS_MR_ESI,
    EXCEPT_IPC_SYS_MR_EDI,
    EXCEPT_IPC_SYS_MR_EBP,
    EXCEPT_IPC_SYS_MR_EIP,
    EXCEPT_IPC_SYS_MR_ESP,
    EXCEPT_IPC_SYS_MR_EFLAGS,
    EXCEPT_IPC_SYS_MR_SYSCALL,
    SEL4_EXCEPT_IPC_LENGTH,
} seL4_ExceptIPCRegister;

typedef union {
    struct {
        seL4_Word fault_ip;
        seL4_Word esp;
        seL4_Word eflags;
        seL4_Word exception_number;
        seL4_Word exception_code;
    } regs;
    seL4_Word raw[5];
} seL4_UserExceptionIpcRegisters;

typedef union {
    struct {
        seL4_Word eax;
        seL4_Word ebx;
        seL4_Word ecx;
        seL4_Word edx;
        seL4_Word esi;
        seL4_Word edi;
        seL4_Word ebp;
        seL4_Word eip;
        seL4_Word esp;
        seL4_Word eflags;
        seL4_Word syscall;
    } regs;
    seL4_Word raw[11];
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
