/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/macros.h>

#ifndef __ASSEMBLER__

#define EXCEPT_IPC_LENGTH SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_Length)

typedef seL4_Word seL4_UserExceptIPCRegister SEL4_DEPRECATED("use seL4_UserException_Msg");

#define EXCEPT_IPC_USER_MR_FAULT_IP  SEL4_DEPRECATE_MACRO(seL4_UserException_FaultIP)
#define EXCEPT_IPC_USER_MR_ESP SEL4_DEPRECATE_MACRO(seL4_UserException_SP)
#define EXCEPT_IPC_USER_MR_EFLAGS SEL4_DEPRECATE_MACRO(seL4_UserException_EFLAGS)
#define EXCEPT_IPC_USER_MR_NUMBER SEL4_DEPRECATE_MACRO(seL4_UserException_Number)
#define EXCEPT_IPC_USER_MR_CODE SEL4_DEPRECATE_MACRO(seL4_UserException_Code)
#define SEL4_USER_EXCEPT_IPC_LENGTH

typedef seL4_Word seL4_ExceptIPCRegister SEL4_DEPRECATED("use seL4_UnknownSyscall_Msg");

#define EXCEPT_IPC_SYS_MR_EAX SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_EAX)
#define EXCEPT_IPC_SYS_MR_EBX SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_EBX)
#define EXCEPT_IPC_SYS_MR_ECX SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_ECX)
#define EXCEPT_IPC_SYS_MR_EDX SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_EDX)
#define EXCEPT_IPC_SYS_MR_ESI SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_ESI)
#define EXCEPT_IPC_SYS_MR_EDI SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_EDI)
#define EXCEPT_IPC_SYS_MR_EBP SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_EBP)
#define EXCEPT_IPC_SYS_MR_EIP SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_FaultIP)
#define EXCEPT_IPC_SYS_MR_ESP SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_SP)
#define EXCEPT_IPC_SYS_MR_EFLAGS SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_FLAGS)
#define EXCEPT_IPC_SYS_MR_SYSCALL SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_Syscall)
#define SEL4_EXCEPT_IPC_LENGTH SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_Length)

typedef union {
    struct {
        seL4_Word fault_ip;
        seL4_Word esp;
        seL4_Word eflags;
        seL4_Word exception_number;
        seL4_Word exception_code;
    } regs;
    seL4_Word raw[5];
}   seL4_UserExceptionIpcRegisters SEL4_DEPRECATED("Use seL4_Fault_t");

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
} seL4_ExceptionIpcRegisters SEL4_DEPRECATED("Use seL4_Fault_t");


static inline SEL4_DEPRECATED("") seL4_Bool seL4_Fault_isWriteFault(seL4_Word FaultStatusRegister)
{
    return (FaultStatusRegister & 0x2);
}

static inline SEL4_DEPRECATED("") seL4_Bool seL4_Fault_isReadFault(seL4_Word FaultStatusRegister)
{
    return !(FaultStatusRegister & 0x2);
}

static inline SEL4_DEPRECATED("") seL4_Bool seL4_Fault_isProtectionFault(seL4_Word FaultStatusRegister)
{
    return (FaultStatusRegister & 0x1);
}

static inline SEL4_DEPRECATED("") seL4_Bool seL4_Fault_isNonPresentFault(seL4_Word FaultStatusRegister)
{
    return !(FaultStatusRegister & 0x1);
}

#endif /*__ASSEMBLER__*/
