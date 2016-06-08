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

#include <benchmark_track.h>
#include <arch/api/syscall.h>
#include <model/statedata.h>

#ifdef CONFIG_PRINTING

extern kernel_entry_t ksKernelEntry;

static inline void
debug_printKernelEntryReason(void)
{
    switch (ksKernelEntry.path) {
    case Entry_Interrupt:
        printf("Interrupt, irq %lu\n", (unsigned long) ksKernelEntry.word);
        break;
    case Entry_UnknownSyscall:
        printf("Unknown syscall, word: %lu", (unsigned long) ksKernelEntry.word);
        break;
    case Entry_VMFault:
        printf("VM Fault, fault type: %lu\n", (unsigned long) ksKernelEntry.word);
        break;
    case Entry_UserLevelFault:
        printf("User level fault, number: %lu", (unsigned long) ksKernelEntry.word);
        break;
    case Entry_Syscall:
        printf("Syscall, number: %ld\n", (long) ksKernelEntry.syscall_no);
        if (ksKernelEntry.syscall_no == SysSend ||
                ksKernelEntry.syscall_no == SysNBSend ||
                ksKernelEntry.syscall_no == SysCall) {

            printf("Cap type: %lu, Invocation tag: %lu\n", (unsigned long) ksKernelEntry.cap_type,
                   (unsigned long) ksKernelEntry.invocation_tag);
        }
    }
}
#endif
#endif /* __API_DEBUG_H */
#endif /* DEBUG */

