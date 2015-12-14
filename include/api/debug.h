/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

#ifdef DEBUG
#ifndef __API_DEBUG_H
#define __API_DEBUG_H

#include <arch/api/syscall.h>

/* the following code can be used at any point in the kernel
 * to determine detail about the kernel entry point */
typedef enum {
    Debug_Interrupt,
    Debug_UnknownSyscall,
    Debug_UserLevelFault,
    Debug_VMFault,
    Debug_Syscall
} debug_entry_type_t;

typedef struct {
    debug_entry_type_t path;
    union {
        struct {
            word_t irq;
        };
        struct {
            word_t word;
        };
        struct {
            word_t number;
            word_t code;
        };
        struct {
            word_t fault_type;
        };
        struct {
            word_t syscall_no;
            word_t cap_type;
            word_t invocation_tag;
        };
    };
} debug_entry_t;

extern debug_entry_t ksKernelEntry;

static inline void
debug_printKernelEntryReason(void)
{
    switch (ksKernelEntry.path) {
    case Debug_Interrupt:
        printf("Interrupt, irq %lu\n", ksKernelEntry.irq);
        break;
    case Debug_UnknownSyscall:
        printf("Unknown syscall, word: %lu", ksKernelEntry.word);
        break;
    case Debug_VMFault:
        printf("VM Fault, fault type: %lu\n", ksKernelEntry.fault_type);
        break;
    case Debug_UserLevelFault:
        printf("User level fault, number: %lu, code: %lu",
               ksKernelEntry.number, ksKernelEntry.code);
        break;
    case Debug_Syscall:
        printf("Syscall, numer: %ld\n", (long) ksKernelEntry.syscall_no);
        if (ksKernelEntry.syscall_no == SysSend ||
                ksKernelEntry.syscall_no == SysNBSend ||
                ksKernelEntry.syscall_no == SysCall) {

            printf("Cap type: %lu, Invocation tag: %lu\n", ksKernelEntry.cap_type,
                   ksKernelEntry.invocation_tag);
        }
    }
}

#endif /* __API_DEBUG_H */
#endif /* DEBUG */

