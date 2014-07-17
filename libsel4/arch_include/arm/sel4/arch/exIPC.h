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

#define SEL4_EXCEPT_IPC_LABEL      3
#define SEL4_USER_EXCEPTION_LABEL  4
#define SEL4_USER_EXCEPTION_LENGTH 5

/*
 * Exception format for an Unknown system call exception.
 */
typedef enum {
    EXCEPT_IPC_SYS_MR_R0,
    EXCEPT_IPC_SYS_MR_R1,
    EXCEPT_IPC_SYS_MR_R2,
    EXCEPT_IPC_SYS_MR_R3,
    EXCEPT_IPC_SYS_MR_R4,
    EXCEPT_IPC_SYS_MR_R5,
    EXCEPT_IPC_SYS_MR_R6,
    EXCEPT_IPC_SYS_MR_R7,
    EXCEPT_IPC_SYS_MR_PC,
    EXCEPT_IPC_SYS_MR_SP,
    EXCEPT_IPC_SYS_MR_LR,
    EXCEPT_IPC_SYS_MR_CPSR,
    EXCEPT_IPC_SYS_MR_SYSCALL,
    SEL4_EXCEPT_IPC_LENGTH
} seL4_ExceptIPCRegister;

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
