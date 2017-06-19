/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_SEL4_DEPRECATED_H
#define __LIBSEL4_SEL4_ARCH_SEL4_DEPRECATED_H

#include <autoconf.h>
#include <sel4/macros.h>

typedef seL4_Word seL4_ExceptIPCRegister SEL4_DEPRECATED("use seL4_UnknownSyscall_Msg");

#define EXCEPT_IPC_SYS_MR_R0 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R0)
#define EXCEPT_IPC_SYS_MR_R1 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R1)
#define EXCEPT_IPC_SYS_MR_R2 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R2)
#define EXCEPT_IPC_SYS_MR_R3 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R3)
#define EXCEPT_IPC_SYS_MR_R4 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R4)
#define EXCEPT_IPC_SYS_MR_R5 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R5)
#define EXCEPT_IPC_SYS_MR_R6 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R6)
#define EXCEPT_IPC_SYS_MR_R7 SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_R7)
#define EXCEPT_IPC_SYS_MR_PC SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_FaultIP)
#define EXCEPT_IPC_SYS_MR_SP SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_SP)
#define EXCEPT_IPC_SYS_MR_LR SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_LR)
#define EXCEPT_IPC_SYS_MR_CPSR SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_CPSR)
#define EXCEPT_IPC_SYS_MR_SYSCALL SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_Syscall)
#define EXCEPT_IPC_LENGTH SEL4_DEPRECATE_MACRO(seL4_UnknownSyscall_Length)

#endif /* __ARCH_SEL4_DEPRECATED_H__ */
