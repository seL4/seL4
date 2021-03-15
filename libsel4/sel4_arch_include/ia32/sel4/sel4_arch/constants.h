/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/macros.h>

#define TLS_GDT_ENTRY 6
#define TLS_GDT_SELECTOR ((TLS_GDT_ENTRY << 3) | 3)

#define IPCBUF_GDT_ENTRY 7
#define IPCBUF_GDT_SELECTOR ((IPCBUF_GDT_ENTRY << 3) | 3)

#define seL4_DataFault 0
#define seL4_InstructionFault 1

#define seL4_PageBits        12 /* 4K */
#define seL4_SlotBits         4
#define seL4_TCBBits         11
#define seL4_EndpointBits     4
#ifdef CONFIG_KERNEL_MCS
#define seL4_NotificationBits 5
#define seL4_ReplyBits        4
#else
#define seL4_NotificationBits 4
#endif

#define seL4_PageTableBits   12
#define seL4_PageTableEntryBits 2
#define seL4_PageTableIndexBits 10

#define seL4_PageDirBits     12
#define seL4_PageDirEntryBits 2
#define seL4_PageDirIndexBits 10
#define seL4_VSpaceBits seL4_PageDirBits

#define seL4_IOPageTableBits 12
#define seL4_NumASIDPoolsBits 2
#define seL4_ASIDPoolBits    12
#define seL4_ASIDPoolIndexBits 10
#define seL4_WordSizeBits 2

#define seL4_HugePageBits    30 /* 1GB */
#define seL4_PDPTBits         0
#define seL4_LargePageBits    22 /* 4MB */

#ifndef __ASSEMBLER__
SEL4_SIZE_SANITY(seL4_PageTableEntryBits, seL4_PageTableIndexBits, seL4_PageTableBits);
SEL4_SIZE_SANITY(seL4_PageDirEntryBits, seL4_PageDirIndexBits, seL4_PageDirBits);
SEL4_SIZE_SANITY(seL4_WordSizeBits, seL4_ASIDPoolIndexBits, seL4_ASIDPoolBits);
#endif

/* Previously large frames were explicitly assumed to be 4M. If not using
 * PAE assuming a legacy environment and leave the old definition */
#define seL4_4MBits           seL4_LargePageBits

#define seL4_WordBits (sizeof(seL4_Word) * 8)

/* Untyped size limits */
#define seL4_MinUntypedBits 4
#define seL4_MaxUntypedBits 29

#ifdef CONFIG_ENABLE_BENCHMARKS
/* size of kernel log buffer in bytes */
#define seL4_LogBufferSize (LIBSEL4_BIT(20))
#endif /* CONFIG_ENABLE_BENCHMARKS */

#ifndef __ASSEMBLER__
/* format of a vm fault message */
typedef enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VMFault_Msg),
} seL4_VMFault_Msg;

typedef enum {
    seL4_UnknownSyscall_EAX,
    seL4_UnknownSyscall_EBX,
    seL4_UnknownSyscall_ECX,
    seL4_UnknownSyscall_EDX,
    seL4_UnknownSyscall_ESI,
    seL4_UnknownSyscall_EDI,
    seL4_UnknownSyscall_EBP,
    seL4_UnknownSyscall_FaultIP,
    seL4_UnknownSyscall_SP,
    seL4_UnknownSyscall_FLAGS,
    seL4_UnknownSyscall_Syscall,
    seL4_UnknownSyscall_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UnknownSyscall_Msg),
} seL4_UnknownSyscall_Msg;

typedef enum {
    seL4_UserException_FaultIP,
    seL4_UserException_SP,
    seL4_UserException_FLAGS,
    seL4_UserException_Number,
    seL4_UserException_Code,
    seL4_UserException_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UserException_Msg)
} seL4_UserException_Msg;

#ifdef CONFIG_KERNEL_MCS
typedef enum {
    seL4_Timeout_Data,
    /* consumed is 64 bits */
    seL4_Timeout_Consumed_HighBits,
    seL4_Timeout_Consumed_LowBits,
    seL4_Timeout_Length,
    SEL4_FORCE_LONG_ENUM(seL4_Timeout_Msg),
} seL4_Timeout_Msg;

typedef enum {
    seL4_TimeoutReply_FaultIP,
    seL4_TimeoutReply_SP,
    seL4_TimeoutReply_FLAGS,
    seL4_TimeoutReply_EAX,
    seL4_TimeoutReply_EBX,
    seL4_TimeoutReply_ECX,
    seL4_TimeoutReply_EDX,
    seL4_TimeoutReply_ESI,
    seL4_TimeoutReply_EDI,
    seL4_TimeoutReply_EBP,
    seL4_TimeoutReply_FS_BASE,
    seL4_TimeoutReply_GS_BASE,
    seL4_TimeoutReply_Length,
    SEL4_FORCE_LONG_ENUM(seL4_TimeoutReply_Msg)
} seL4_TimeoutReply_Msg;
#endif
#endif /* __ASSEMBLER__ */
#ifdef CONFIG_KERNEL_MCS
#define seL4_FastMessageRegisters 1
#else
#define seL4_FastMessageRegisters 2
#endif

/* IPC buffer is 512 bytes, giving size bits of 9 */
#define seL4_IPCBufferSizeBits 9

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xe0000000
