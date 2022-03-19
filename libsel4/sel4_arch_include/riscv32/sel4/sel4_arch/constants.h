/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/macros.h>

#define seL4_WordBits           32
/* log 2 bits in a word */
#define seL4_WordSizeBits       2

#define seL4_SlotBits           4
#ifdef CONFIG_KERNEL_MCS
#define seL4_NotificationBits   5
#define seL4_ReplyBits          4
#else
#define seL4_NotificationBits   4
#endif
#define seL4_EndpointBits       4
#define seL4_IPCBufferSizeBits  9
#define seL4_TCBBits            9

/* Untyped size limits */
#define seL4_MinUntypedBits     4
#define seL4_MaxUntypedBits     29

/* RISC-V Sv32 pages/ptes sizes */
#define seL4_PageTableEntryBits 2
#define seL4_PageTableIndexBits 10

#define seL4_PageBits           12
#define seL4_LargePageBits      22
#define seL4_PageTableBits      12
#define seL4_VSpaceBits         seL4_PageTableBits

#define seL4_NumASIDPoolsBits    5
#define seL4_ASIDPoolIndexBits  4
#define seL4_ASIDPoolBits       12
#ifndef __ASSEMBLER__

typedef enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
} seL4_VMFault_Msg;

typedef enum {
    seL4_UnknownSyscall_FaultIP,
    seL4_UnknownSyscall_SP,
    seL4_UnknownSyscall_RA,
    seL4_UnknownSyscall_A0,
    seL4_UnknownSyscall_A1,
    seL4_UnknownSyscall_A2,
    seL4_UnknownSyscall_A3,
    seL4_UnknownSyscall_A4,
    seL4_UnknownSyscall_A5,
    seL4_UnknownSyscall_A6,
    seL4_UnknownSyscall_Syscall,
    seL4_UnknownSyscall_Length,
} seL4_UnknownSyscall_Msg;

typedef enum {
    seL4_UserException_FaultIP,
    seL4_UserException_SP,
    seL4_UserException_Number,
    seL4_UserException_Code,
    seL4_UserException_Length,
} seL4_UserException_Msg;

#ifdef CONFIG_KERNEL_MCS
typedef enum {
    seL4_TimeoutReply_FaultIP,
    seL4_TimeoutReply_LR,
    seL4_TimeoutReply_SP,
    seL4_TimeoutReply_GP,
    seL4_TimeoutReply_s0,
    seL4_TimeoutReply_s1,
    seL4_TimeoutReply_s2,
    seL4_TimeoutReply_s3,
    seL4_TimeoutReply_s4,
    seL4_TimeoutReply_s5,
    seL4_TimeoutReply_s6,
    seL4_TimeoutReply_s7,
    seL4_TimeoutReply_s8,
    seL4_TimeoutReply_s9,
    seL4_TimeoutReply_s10,
    seL4_TimeoutReply_s11,
    seL4_TimeoutReply_a0,
    seL4_TimeoutReply_a1,
    seL4_TimeoutReply_a2,
    seL4_TimeoutReply_a3,
    seL4_TimeoutReply_a4,
    seL4_TimeoutReply_a5,
    seL4_TimeoutReply_a6,
    seL4_TimeoutReply_a7,
    seL4_TimeoutReply_t0,
    seL4_TimeoutReply_t1,
    seL4_TimeoutReply_t2,
    seL4_TimeoutReply_t3,
    seL4_TimeoutReply_t4,
    seL4_TimeoutReply_t5,
    seL4_TimeoutReply_t6,
    seL4_TimeoutReply_TP,
    seL4_TimeoutReply_Length,
} seL4_TimeoutReply_Msg;

typedef enum {
    seL4_Timeout_Data,
    seL4_Timeout_Consumed_HighBits,
    seL4_Timeout_Consumed_LowBits,
    seL4_Timeout_Length,
} seL4_TimeoutMsg;
#endif
#endif /* __ASSEMBLER__ */

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0x80000000lu

#ifdef CONFIG_ENABLE_BENCHMARKS
/* size of kernel log buffer in bytes */
#define seL4_LogBufferSize (LIBSEL4_BIT(20))
#endif /* CONFIG_ENABLE_BENCHMARKS */
