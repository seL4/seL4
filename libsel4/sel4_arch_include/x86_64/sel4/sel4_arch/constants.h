/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(D61_BSD)
 */

#ifndef __LIBSEL4_SEL4_SEL4_ARCH_CONSTANTS_H_
#define __LIBSEL4_SEL4_SEL4_ARCH_CONSTANTS_H_

#include <autoconf.h>

#define TLS_GDT_ENTRY   7
#define TLS_GDT_SELECTOR ((TLS_GDT_ENTRY << 3) | 3)

#define IPCBUF_GDT_ENTRY    8
#define IPCBUF_GDT_SELECTOR ((IPCBUF_GDT_ENTRY << 3) | 3)

#define seL4_DataFault 0
#define seL4_InstructionFault 1

/* for x86-64, the large page size is 2 MiB and huge page size is 1 GiB */
#define seL4_WordBits           64
#define seL4_PageBits           12
#define seL4_SlotBits           5
#define seL4_TCBBits            11
#define seL4_EndpointBits       4
#define seL4_NotificationBits   5
#define seL4_PageTableBits      12
#define seL4_PageDirBits        12
#define seL4_PDPTBits           12
#define seL4_PML4Bits           12
#define seL4_IOPageTableBits    12
#define seL4_LargePageBits      21
#define seL4_HugePageBits       30
#define seL4_ASIDPoolBits       12

/* Untyped size limits */
#define seL4_MinUntypedBits 4
#define seL4_MaxUntypedBits 47

#ifndef __ASSEMBLER__
enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
    SEL4_FORCE_LONG_ENUM(seL4_VMFault_Msg),
} seL4_VMFault_Msg;

enum {
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
#endif /* __LIBSEL4_SEL4_SEL4_ARCH_CONSTANTS_H_ */
