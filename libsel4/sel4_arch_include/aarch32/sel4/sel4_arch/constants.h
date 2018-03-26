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

#ifndef __LIBSEL4_SEL4_ARCH_CONSTANTS_H
#define __LIBSEL4_SEL4_ARCH_CONSTANTS_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#ifndef __ASSEMBLER__
#ifdef CONFIG_IPC_BUF_GLOBALS_FRAME
enum {
    seL4_GlobalsFrame = 0xffffc000,
};
#endif /* CONFIG_IPC_BUF_GLOBALS_FRAME */

/* format of an unknown syscall message */
enum {
    seL4_UnknownSyscall_R0,
    seL4_UnknownSyscall_R1,
    seL4_UnknownSyscall_R2,
    seL4_UnknownSyscall_R3,
    seL4_UnknownSyscall_R4,
    seL4_UnknownSyscall_R5,
    seL4_UnknownSyscall_R6,
    seL4_UnknownSyscall_R7,
    seL4_UnknownSyscall_FaultIP,
    seL4_UnknownSyscall_SP,
    seL4_UnknownSyscall_LR,
    seL4_UnknownSyscall_CPSR,
    seL4_UnknownSyscall_Syscall,
    /* length of an unknown syscall message */
    seL4_UnknownSyscall_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UnknownSyscall_Msg),
} seL4_UnknownSyscall_Msg;

/* format of a user exception message */
enum {
    seL4_UserException_FaultIP,
    seL4_UserException_SP,
    seL4_UserException_CPSR,
    seL4_UserException_Number,
    seL4_UserException_Code,
    /* length of a user exception */
    seL4_UserException_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UserException_Msg),
} seL4_UserException_Msg;

/* format of a vm fault message */
enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VMFault_Msg),
} seL4_VMFault_Msg;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
enum {
    seL4_VGICMaintenance_IDX,
    seL4_VGICMaintenance_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VGICMaintenance_Msg),
} seL4_VGICMaintenance_Msg;

enum {
    seL4_VCPUFault_HSR,
    seL4_VCPUFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VCPUFault_Msg),
} seL4_VCPUFault_Msg;

enum {
    seL4_VCPUReg_SCTLR = 0,
    seL4_VCPUReg_LRsvc,
    seL4_VCPUReg_SPsvc,
    seL4_VCPUReg_LRabt,
    seL4_VCPUReg_SPabt,
    seL4_VCPUReg_LRund,
    seL4_VCPUReg_SPund,
    seL4_VCPUReg_LRirq,
    seL4_VCPUReg_SPirq,
    seL4_VCPUReg_LRfiq,
    seL4_VCPUReg_SPfiq,
    seL4_VCPUReg_R8fiq,
    seL4_VCPUReg_R9fiq,
    seL4_VCPUReg_R10fiq,
    seL4_VCPUReg_R11fiq,
    seL4_VCPUReg_R12fiq,
    seL4_VCPUReg_Num,
} seL4_VCPUReg;

#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
#endif /* !__ASSEMBLER__ */

#define seL4_DataFault 0
#define seL4_InstructionFault 1
/* object sizes - 2^n */
#define seL4_PageBits 12
#define seL4_LargePageBits 16
#define seL4_SlotBits 4

#if ((defined(CONFIG_DEBUG_BUILD) || CONFIG_MAX_NUM_NODES > 1) && defined(CONFIG_HAVE_FPU) && defined(CONFIG_HARDWARE_DEBUG_API)) \
    || (defined(CONFIG_HAVE_FPU) && defined(CONFIG_ARM_HYPERVISOR_SUPPORT))
#define seL4_TCBBits 11
#elif defined(CONFIG_HAVE_FPU)
#define seL4_TCBBits 10
#else
#define seL4_TCBBits 9
#endif

#define seL4_EndpointBits 4
#define seL4_NotificationBits 4

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define seL4_PageTableBits 12
#define seL4_PageTableEntryBits 3
#define seL4_PageTableIndexBits 9
#define seL4_SectionBits 21
#define seL4_SuperSectionBits 25
#define seL4_PGDEntryBits 3
#define seL4_PGDIndexBits 2
#define seL4_PGDBits (seL4_PGDIndexBits + seL4_PGDEntryBits)
#define seL4_PageDirEntryBits 3
#define seL4_PageDirIndexBits 11
#define seL4_VCPUBits 12
#else
#define seL4_PageTableBits 10
#define seL4_PageTableEntryBits 2
#define seL4_PageTableIndexBits 8
#define seL4_SectionBits 20
#define seL4_SuperSectionBits 24
#define seL4_PageDirEntryBits 2
#define seL4_PageDirIndexBits 12
#endif

#define seL4_PageDirBits 14

#define seL4_ASIDPoolBits 12
#define seL4_ASIDPoolIndexBits 10
#define seL4_ARM_VCPUBits       12
#define seL4_IOPageTableBits    12

/* bits in a word */
#define seL4_WordBits (sizeof(seL4_Word) * 8)
/* log 2 bits in a word */
#define seL4_WordSizeBits 2

#ifndef __ASSEMBLER__
SEL4_SIZE_SANITY(seL4_PageTableEntryBits, seL4_PageTableIndexBits, seL4_PageTableBits);
SEL4_SIZE_SANITY(seL4_PageDirEntryBits,   seL4_PageDirIndexBits,   seL4_PageDirBits);
SEL4_SIZE_SANITY(seL4_WordSizeBits, seL4_ASIDPoolIndexBits, seL4_ASIDPoolBits);
#endif

/* Untyped size limits */
#define seL4_MinUntypedBits 4
#define seL4_MaxUntypedBits 29

#ifdef CONFIG_ENABLE_BENCHMARKS
/* size of kernel log buffer in bytes */
#define seL4_LogBufferSize (LIBSEL4_BIT(20))
#endif /* CONFIG_ENABLE_BENCHMARKS */

#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_FirstBreakpoint (0)
#define seL4_FirstDualFunctionMonitor (-1)
#define seL4_NumDualFunctionMonitors (0)
#endif

#define seL4_FastMessageRegisters 4

/* IPC buffer is 512 bytes, giving size bits of 9 */
#define seL4_IPCBufferSizeBits 9

#endif
