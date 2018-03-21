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
#define seL4_NotificationBits 4

#define seL4_PageTableBits   12
#define seL4_PageTableEntryBits 2
#define seL4_PageTableIndexBits 10

#define seL4_PageDirBits     12
#define seL4_PageDirEntryBits 2
#define seL4_PageDirIndexBits 10

#define seL4_IOPageTableBits 12
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
enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VMFault_Msg),
} seL4_VMFault_Msg;

enum {
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

enum {
    seL4_UserException_FaultIP,
    seL4_UserException_SP,
    seL4_UserException_FLAGS,
    seL4_UserException_Number,
    seL4_UserException_Code,
    seL4_UserException_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UserException_Msg)
} seL4_UserException_Msg;

#endif /* __ASSEMBLER__ */
#define seL4_FastMessageRegisters 2

/* IPC buffer is 512 bytes, giving size bits of 9 */
#define seL4_IPCBufferSizeBits 9

#endif
