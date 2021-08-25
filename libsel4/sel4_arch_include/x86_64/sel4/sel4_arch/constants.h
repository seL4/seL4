/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/macros.h>

#define TLS_GDT_ENTRY   7
#define TLS_GDT_SELECTOR ((TLS_GDT_ENTRY << 3) | 3)

#define IPCBUF_GDT_ENTRY    8
#define IPCBUF_GDT_SELECTOR ((IPCBUF_GDT_ENTRY << 3) | 3)

#define seL4_DataFault 0
#define seL4_InstructionFault 1

/* for x86-64, the large page size is 2 MiB and huge page size is 1 GiB */
#define seL4_WordBits           64
#define seL4_WordSizeBits       3
#define seL4_PageBits           12
#define seL4_SlotBits           5
#if CONFIG_XSAVE_SIZE >= 832
#define seL4_TCBBits            12
#else
#define seL4_TCBBits            11
#endif
#define seL4_EndpointBits       4
#ifdef CONFIG_KERNEL_MCS
#define seL4_NotificationBits   6
#define seL4_ReplyBits          5
#else
#define seL4_NotificationBits   5
#endif

#define seL4_PageTableBits      12
#define seL4_PageTableEntryBits 3
#define seL4_PageTableIndexBits 9

#define seL4_PageDirBits        12
#define seL4_PageDirEntryBits   3
#define seL4_PageDirIndexBits   9

#define seL4_PDPTBits           12
#define seL4_PDPTEntryBits      3
#define seL4_PDPTIndexBits      9

#define seL4_PML4Bits           12
#define seL4_PML4EntryBits      3
#define seL4_PML4IndexBits      9
#define seL4_VSpaceBits seL4_PML4Bits

#define seL4_IOPageTableBits    12
#define seL4_LargePageBits      21
#define seL4_HugePageBits       30
#define seL4_NumASIDPoolsBits    3
#define seL4_ASIDPoolBits       12
#define seL4_ASIDPoolIndexBits 9

/* Untyped size limits */
#define seL4_MinUntypedBits 4
#define seL4_MaxUntypedBits 47

#ifndef __ASSEMBLER__

SEL4_SIZE_SANITY(seL4_PageTableEntryBits, seL4_PageTableIndexBits, seL4_PageTableBits);
SEL4_SIZE_SANITY(seL4_PageDirEntryBits, seL4_PageDirIndexBits, seL4_PageDirBits);
SEL4_SIZE_SANITY(seL4_PDPTEntryBits, seL4_PDPTIndexBits, seL4_PDPTBits);
SEL4_SIZE_SANITY(seL4_PML4EntryBits, seL4_PML4IndexBits, seL4_PML4Bits);
SEL4_SIZE_SANITY(seL4_WordSizeBits, seL4_ASIDPoolIndexBits, seL4_ASIDPoolBits);

typedef enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VMFault_Msg),
} seL4_VMFault_Msg;

typedef enum {
    seL4_UnknownSyscall_RAX,
    seL4_UnknownSyscall_RBX,
    seL4_UnknownSyscall_RCX,
    seL4_UnknownSyscall_RDX,
    seL4_UnknownSyscall_RSI,
    seL4_UnknownSyscall_RDI,
    seL4_UnknownSyscall_RBP,
    seL4_UnknownSyscall_R8,
    seL4_UnknownSyscall_R9,
    seL4_UnknownSyscall_R10,
    seL4_UnknownSyscall_R11,
    seL4_UnknownSyscall_R12,
    seL4_UnknownSyscall_R13,
    seL4_UnknownSyscall_R14,
    seL4_UnknownSyscall_R15,
    seL4_UnknownSyscall_FaultIP,
    seL4_UnknownSyscall_SP,
    seL4_UnknownSyscall_FLAGS,
    seL4_UnknownSyscall_Syscall,
    seL4_UnknownSyscall_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UnknownSyscall_Msg)
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
    seL4_Timeout_Consumed,
    seL4_Timeout_Length,
    SEL4_FORCE_LONG_ENUM(seL4_Timeout_Msg)
} seL4_TimeoutMsg;

typedef enum {
    seL4_TimeoutReply_FaultIP,
    seL4_TimeoutReply_RSP,
    seL4_TimeoutReply_FLAGS,
    seL4_TimeoutReply_RAX,
    seL4_TimeoutReply_RBX,
    seL4_TimeoutReply_RCX,
    seL4_TimeoutReply_RDX,
    seL4_TimeoutReply_RSI,
    seL4_TimeoutReply_RDI,
    seL4_TimeoutReply_RBP,
    seL4_TimeoutReply_R8,
    seL4_TimeoutReply_R9,
    seL4_TimeoutReply_R10,
    seL4_TimeoutReply_R11,
    seL4_TimeoutReply_R12,
    seL4_TimeoutReply_R13,
    seL4_TimeoutReply_R14,
    seL4_TimeoutReply_R15,
    seL4_TimeoutReply_TLS_BASE,
    seL4_TimeoutReply_Length,
    SEL4_FORCE_LONG_ENUM(seL4_TimeoutReply_Msg)
} seL4_TimeoutReply_Msg;
#endif
#endif /* __ASSEMBLER__ */
#define seL4_FastMessageRegisters 4

/* IPC buffer is 1024 bytes, giving size bits of 10 */
#define seL4_IPCBufferSizeBits 10

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0x00007ffffffff000

