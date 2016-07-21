/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_CONSTANTS_H
#define __LIBSEL4_SEL4_ARCH_CONSTANTS_H

#include <autoconf.h>

#define TLS_GDT_ENTRY 6
#define TLS_GDT_SELECTOR ((TLS_GDT_ENTRY << 3) | 3)

#define IPCBUF_GDT_ENTRY 7
#define IPCBUF_GDT_SELECTOR ((IPCBUF_GDT_ENTRY << 3) | 3)

#define seL4_PageBits        12
#define seL4_SlotBits         4
#define seL4_TCBBits         10
#define seL4_EndpointBits     4
#define seL4_NotificationBits 5
#define seL4_PageTableBits   12
#define seL4_PageDirBits     12
#define seL4_IOPageTableBits 12
#define seL4_ASIDPoolBits    12
#define seL4_SchedContextBits 6

#ifdef CONFIG_PAE_PAGING
#define seL4_PDPTBits         5
#define seL4_LargePageBits    21
#else
#define seL4_PDPTBits         0
#define seL4_LargePageBits    22
#endif

/* Previously large frames were explicitly assumed to be 4M. If not using
 * PAE assuming a legacy environment and leave the old definition */
#ifndef CONFIG_PAE_PAGING
#define seL4_4MBits           seL4_LargePageBits
#endif

#define seL4_WordBits (sizeof(seL4_Word) * 8)

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
    seL4_UnknownSyscall_EFLAGS,
    seL4_UnknownSyscall_Syscall,
    seL4_UnknownSyscall_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UnknownSyscall_Msg),
} seL4_UnknownSyscall_Msg;

enum {
    seL4_UserException_FaultIP,
    seL4_UserException_SP,
    seL4_UserException_EFLAGS,
    seL4_UserException_Number,
    seL4_UserException_Code,
    seL4_UserException_Length,
    SEL4_FORCE_LONG_ENUM(seL4_UserException_Msg)
} seL4_UserException_Msg;

enum {
    seL4_TimeoutFault_Data,
    seL4_TimeoutFault_Consumed,
    /* consumed is 64 bits */
    seL4_TimeoutFault_Consumed_LowBits,
    seL4_TimeoutFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_TimeoutFault_Msg),
} seL4_TimeoutFault_Msg;

enum {
    seL4_TimeoutFaultReply_FaultIP,
    seL4_TimeoutFaultReply_SP,
    seL4_TimeoutFaultReply_EFLAGS,
    seL4_TimeoutFaultReply_EAX,
    seL4_TimeoutFaultReply_EBX,
    seL4_TimeoutFaultReply_ECX,
    seL4_TimeoutFaultReply_EDX,
    seL4_TimeoutFaultReply_ESI,
    seL4_TimeoutFaultReply_EDI,
    seL4_TimeoutFaultReply_EBP,
    seL4_TimeoutFaultReply_TLS_BASE,
    seL4_TimeoutFaultReply_FS,
    seL4_TimeoutFaultReply_GS,
    seL4_TimeoutFaultReply_Length,
    SEL4_FORCE_LONG_ENUM(seL4_TimeoutFaultReply_Msg)
} seL4_TimeoutFaultReply_Msg;

#endif /* __ASSEMBLER__ */
#endif
