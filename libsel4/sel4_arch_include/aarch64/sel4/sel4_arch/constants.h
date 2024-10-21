/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/macros.h>

#ifndef __ASSEMBLER__
/* format of an unknown syscall message */
typedef enum {
    seL4_UnknownSyscall_X0,
    seL4_UnknownSyscall_X1,
    seL4_UnknownSyscall_X2,
    seL4_UnknownSyscall_X3,
    seL4_UnknownSyscall_X4,
    seL4_UnknownSyscall_X5,
    seL4_UnknownSyscall_X6,
    seL4_UnknownSyscall_X7,
    seL4_UnknownSyscall_FaultIP,
    seL4_UnknownSyscall_SP,
    seL4_UnknownSyscall_LR,
    seL4_UnknownSyscall_SPSR,
    seL4_UnknownSyscall_Syscall,
    /* length of an unknown syscall message */
    seL4_UnknownSyscall_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UnknownSyscall_Msg),
} seL4_UnknownSyscall_Msg;

/* format of a user exception message */
typedef enum {
    seL4_UserException_FaultIP,
    seL4_UserException_SP,
    seL4_UserException_SPSR,
    seL4_UserException_Number,
    seL4_UserException_Code,
    /* length of a user exception */
    seL4_UserException_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UserException_Msg),
} seL4_UserException_Msg;

/* format of a vm fault message */
typedef enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VMFault_Msg),
} seL4_VMFault_Msg;

/* Placeholder for CHERI fault messages.
 * cheriTODO: introduce a new CHERI fault format and decouple it from VM faults,
 * and expose it to user-level.
 */
typedef enum {
    seL4_CHERIFault_IP,
    seL4_CHERIFault_Addr,
    seL4_CHERIFault_FSR,
    seL4_CHERIFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_CHERIFault_Msg),
} seL4_CHERIFault_Msg;

typedef enum {
    seL4_VGICMaintenance_IDX,
    seL4_VGICMaintenance_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VGICMaintenance_Msg),
} seL4_VGICMaintenance_Msg;

typedef enum {
    seL4_VPPIEvent_IRQ,
    seL4_VPPIEvent_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VPPIEvent_Msg),
} seL4_VPPIEvent_Msg;


typedef enum {
    seL4_VCPUFault_HSR,
    seL4_VCPUFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VCPUFault_Msg),
} seL4_VCPUFault_Msg;

typedef enum {
    /* VM control registers EL1 */
    seL4_VCPUReg_SCTLR = 0,
    seL4_VCPUReg_TTBR0,
    seL4_VCPUReg_TTBR1,
    seL4_VCPUReg_TCR,
    seL4_VCPUReg_MAIR,
    seL4_VCPUReg_AMAIR,
    seL4_VCPUReg_CIDR,

    /* other system registers EL1 */
    seL4_VCPUReg_ACTLR,
    seL4_VCPUReg_CPACR,

    /* exception handling registers EL1 */
    seL4_VCPUReg_AFSR0,
    seL4_VCPUReg_AFSR1,
    seL4_VCPUReg_ESR,
    seL4_VCPUReg_FAR,
    seL4_VCPUReg_ISR,
    seL4_VCPUReg_VBAR,

    /* thread pointer/ID registers EL0/EL1 */
    seL4_VCPUReg_TPIDR_EL1,

    /* Virtualisation Multiprocessor ID Register */
    seL4_VCPUReg_VMPIDR_EL2,

    /* general registers x0 to x30 have been saved by traps.S */
    seL4_VCPUReg_SP_EL1,
    seL4_VCPUReg_ELR_EL1,
    seL4_VCPUReg_SPSR_EL1, // 32-bit

    /* generic timer registers, to be completed */
    seL4_VCPUReg_CNTV_CTL,
    seL4_VCPUReg_CNTV_CVAL,
    seL4_VCPUReg_CNTVOFF,
    seL4_VCPUReg_CNTKCTL_EL1,

    seL4_VCPUReg_Num,
    SEL4_FORCE_LONG_ENUM(seL4_VCPUReg),
} seL4_VCPUReg;

#ifdef CONFIG_KERNEL_MCS
typedef enum {
    seL4_TimeoutReply_FaultIP,
    seL4_TimeoutReply_SP,
    seL4_TimeoutReply_SPSR_EL1,
    seL4_TimeoutReply_X0,
    seL4_TimeoutReply_X1,
    seL4_TimeoutReply_X2,
    seL4_TimeoutReply_X3,
    seL4_TimeoutReply_X4,
    seL4_TimeoutReply_X5,
    seL4_TimeoutReply_X6,
    seL4_TimeoutReply_X7,
    seL4_TimeoutReply_X8,
    seL4_TimeoutReply_X16,
    seL4_TimeoutReply_X17,
    seL4_TimeoutReply_X18,
    seL4_TimeoutReply_X29,
    seL4_TimeoutReply_X30,
    seL4_TimeoutReply_X9,
    seL4_TimeoutReply_X10,
    seL4_TimeoutReply_X11,
    seL4_TimeoutReply_X12,
    seL4_TimeoutReply_X13,
    seL4_TimeoutReply_X14,
    seL4_TimeoutReply_X15,
    seL4_TimeoutReply_X19,
    seL4_TimeoutReply_X20,
    seL4_TimeoutReply_X21,
    seL4_TimeoutReply_X22,
    seL4_TimeoutReply_X23,
    seL4_TimeoutReply_X24,
    seL4_TimeoutReply_X25,
    seL4_TimeoutReply_X26,
    seL4_TimeoutReply_X27,
    seL4_TimeoutReply_X28,
    seL4_TimeoutReply_Length,
    SEL4_FORCE_LONG_ENUM(seL4_TimeoutReply_Msg)
} seL4_TimeoutReply_Msg;

typedef enum {
    seL4_Timeout_Data,
    seL4_Timeout_Consumed,
    seL4_Timeout_Length,
    SEL4_FORCE_LONG_ENUM(seL4_Timeout_Msg)
} seL4_Timeout_Msg;
#endif
#endif /* !__ASSEMBLER__ */

#define seL4_DataFault 0
#define seL4_InstructionFault 1
#define seL4_CHERIFault 2

/* object sizes - 2^n */
#define seL4_PageBits 12
#define seL4_LargePageBits 21
#define seL4_HugePageBits 30

#if defined(__CHERI_PURE_CAPABILITY__)
#define seL4_TCBBits 12
#define seL4_SlotBits 7
#define seL4_EndpointBits 7
#define seL4_ASIDPoolIndexBits 7
#define seL4_RegisterSizeBits 5
#else
#if defined(CONFIG_HARDWARE_DEBUG_API) || defined(CONFIG_ARM_HYP_ENABLE_VCPU_CP14_SAVE_AND_RESTORE)
#define seL4_TCBBits 12
#else
#define seL4_TCBBits 11
#endif
#define seL4_SlotBits 5
#define seL4_EndpointBits 4
#define seL4_ASIDPoolIndexBits 9
#define seL4_RegisterSizeBits 3
#endif

#ifdef CONFIG_KERNEL_MCS
#define seL4_NotificationBits 6
#define seL4_ReplyBits           5
#else
#if defined(__CHERI_PURE_CAPABILITY__)
#define seL4_NotificationBits 8
#else
#define seL4_NotificationBits 5
#endif
#endif

#define seL4_PageTableBits 12
#define seL4_PageTableEntryBits 3
#define seL4_PageTableIndexBits 9

#define seL4_NumASIDPoolsBits 7
#define seL4_ASIDPoolBits 12
#define seL4_IOPageTableBits 12
#define seL4_WordSizeBits 3

#define seL4_VSpaceEntryBits 3

#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined (CONFIG_ARM_PA_SIZE_BITS_40)
/* for a 3 level translation, we skip the PGD */

#define seL4_VSpaceBits 13
#define seL4_VSpaceIndexBits 10
#else


#define seL4_VSpaceBits 12
#define seL4_VSpaceIndexBits 9
#endif

#define seL4_ARM_VCPUBits   12
#define seL4_VCPUBits       12

/* word size */
#define seL4_WordBits (sizeof(seL4_Word) * 8)

/* Untyped size limits */
#define seL4_MinUntypedBits 4
#define seL4_MaxUntypedBits 47

#ifndef __ASSEMBLER__
SEL4_SIZE_SANITY(seL4_PageTableEntryBits, seL4_PageTableIndexBits, seL4_PageTableBits);
SEL4_SIZE_SANITY(seL4_RegisterSizeBits, seL4_ASIDPoolIndexBits, seL4_ASIDPoolBits);
SEL4_SIZE_SANITY(seL4_VSpaceEntryBits, seL4_VSpaceIndexBits, seL4_VSpaceBits);
#endif

#ifdef CONFIG_ENABLE_BENCHMARKS
/* size of kernel log buffer in bytes */
#define seL4_LogBufferSize (LIBSEL4_BIT(20))
#endif /* CONFIG_ENABLE_BENCHMARKS */

#define seL4_FastMessageRegisters 4

#if defined(CONFIG_HAVE_CHERI)
/* IPC buffer is 2048 bytes, giving size bits of 11 */
#define seL4_IPCBufferSizeBits 11
#else
/* IPC buffer is 1024 bytes, giving size bits of 10 */
#define seL4_IPCBufferSizeBits 10
#endif

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

/* The userspace occupies the range 0x0 to 0xfffffffffff.
 * The stage-1 translation is disabled, and the stage-2
 * translation input addree size is constrained by the
 * ID_AA64MMFR0_EL1.PARange which is 44 bits on TX1.
 * Anything address above the range above triggers an
 * address size fault.
 */
/* First address in the virtual address space that is not accessible to user level */
#if defined(CONFIG_ARM_PA_SIZE_BITS_44)
#define seL4_UserTop 0x00000fffffffffff
#elif defined(CONFIG_ARM_PA_SIZE_BITS_40)
#define seL4_UserTop 0x000000ffffffffff
#else
#error "Unknown physical address width"
#endif

#else
/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0x00007fffffffffff
#endif
