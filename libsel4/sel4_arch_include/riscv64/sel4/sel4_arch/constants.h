/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __LIBSEL4_SEL4_ARCH_CONSTANTS_H
#define __LIBSEL4_SEL4_ARCH_CONSTANTS_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#define seL4_WordBits           64
/* log 2 bits in a word */
#define seL4_WordSizeBits       3

#define seL4_SlotBits           5
#ifdef CONFIG_KERNEL_MCS
#define seL4_NotificationBits   6
#define seL4_ReplyBits          5
#else
#define seL4_NotificationBits   5
#endif
#define seL4_EndpointBits       4
#define seL4_IPCBufferSizeBits  10
#define seL4_TCBBits            10

/* Sv39/Sv48 pages/ptes sizes */
#define seL4_PageTableEntryBits 3
#define seL4_PageTableIndexBits 9

#define seL4_PageBits          12
#define seL4_LargePageBits     21
#define seL4_HugePageBits      30
#define seL4_TeraPageBits      39
#define seL4_PageTableBits     12
#define seL4_S2RootPageTableBits 14
#ifdef CONFIG_RISCV_HE
#define seL4_VSpaceBits        seL4_S2RootPageTableBits
#else
#define seL4_VSpaceBits        seL4_PageTableBits
#endif

#ifdef CONFIG_RISCV_HE
/* Assume 14-bit VMID for the RISCV64 */
#define seL4_NumASIDPoolsBits   5
#define seL4_ASIDPoolIndexBits  9
#else

#define seL4_NumASIDPoolsBits   7
#define seL4_ASIDPoolIndexBits  9

#endif
#define seL4_ASIDPoolBits       12

/* Untyped size limits */
#define seL4_MinUntypedBits     4
#define seL4_MaxUntypedBits     38

/* VCPU-related definitions */
#define seL4_RISCV_VCPUBits         10
#define seL4_VCPUBits               10
/* The root page table for stage2 translation is 16 KiB */
#define seL4_S2RootPageTableBits    14

#ifndef __ASSEMBLER__

enum {
    seL4_VMFault_IP,
#ifdef CONFIG_RISCV_HE
    seL4_VMFault_Instruction,
#endif
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
} seL4_VMFault_Msg;

#ifdef CONFIG_RISCV_HE
enum {
    seL4_VCPUFault_Cause,
    seL4_VCPUFault_Length,
} seL4_VCPUFault_Msg;
#endif

enum {
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

enum {
    seL4_UserException_FaultIP,
    seL4_UserException_SP,
    seL4_UserException_FLAGS,
    seL4_UserException_Number,
    seL4_UserException_Code,
    seL4_UserException_Length,
} seL4_UserException_Msg;

#ifdef CONFIG_KERNEL_MCS
enum {
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

enum {
    seL4_Timeout_Data,
    seL4_Timeout_Consumed,
    seL4_Timeout_Length,
} seL4_TimeoutMsg;
#endif

#ifdef CONFIG_RISCV_HE

enum {
    seL4_VCPUReg_SSTATUS = 0,
    seL4_VCPUReg_SIE,
    seL4_VCPUReg_STVEC,
    seL4_VCPUReg_SSCRATCH,
    seL4_VCPUReg_SEPC,
    seL4_VCPUReg_SCAUSE,
    seL4_VCPUReg_STVAL,
    seL4_VCPUReg_SIP,
    seL4_VCPUReg_SATP,
    seL4_VCPUReg_TIMER,
    seL4_VCPUReg_Num,
} seL4_VCPUReg;

#endif /* end of CONFIG_RISCV_HE */

#endif /* __ASSEMBLER__ */

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0x0000003ffffff000

#endif
