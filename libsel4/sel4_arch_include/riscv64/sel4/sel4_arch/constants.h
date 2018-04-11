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
#define seL4_NotificationBits   5
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

#define seL4_ASIDPoolIndexBits  10
#define seL4_ASIDPoolBits       12

/* Untyped size limits */
#define seL4_MinUntypedBits     4
#define seL4_MaxUntypedBits     47

enum {
    seL4_VMFault_IP,
    seL4_VMFault_Addr,
    seL4_VMFault_PrefetchFault,
    seL4_VMFault_FSR,
    seL4_VMFault_Length,
} seL4_VMFault_Msg;

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

#endif
