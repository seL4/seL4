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

#ifdef CONFIG_DEBUG_BUILD
#ifndef __API_DEBUG_H
#define __API_DEBUG_H

#include <benchmark/benchmark_track.h>
#include <arch/api/syscall.h>
#include <arch/kernel/vspace.h>
#include <model/statedata.h>
#include <kernel/thread.h>

#ifdef CONFIG_PRINTING

static inline void
debug_printKernelEntryReason(void)
{
    printf("\nKernel entry via ");
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
#ifdef CONFIG_HARDWARE_DEBUG_API
    case Entry_DebugFault:
        printf("Debug fault. Fault Vaddr: 0x%lx", (unsigned long) ksKernelEntry.word);
        break;
#endif
    case Entry_Syscall:
        printf("Syscall, number: %ld, %s\n", (long) ksKernelEntry.syscall_no, syscall_names[ksKernelEntry.syscall_no]);
        if (ksKernelEntry.syscall_no == -SysSend ||
                ksKernelEntry.syscall_no == -SysNBSend ||
                ksKernelEntry.syscall_no == -SysCall) {

            printf("Cap type: %lu, Invocation tag: %lu\n", (unsigned long) ksKernelEntry.cap_type,
                   (unsigned long) ksKernelEntry.invocation_tag);
        }
        break;
#ifdef CONFIG_ARCH_ARM
    case Entry_VCPUFault:
        printf("VCPUFault\n");
        break;
#endif
#ifdef CONFIG_ARCH_x86
    case Entry_VMExit:
        printf("VMExit\n");
        break;
#endif
    default:
        printf("Unknown\n");
        break;

    }
}

/* Prints the user context and stack trace of the current thread */
static inline void
debug_printUserState(void)
{
    tcb_t *tptr = NODE_STATE(ksCurThread);
    printf("Current thread: %s\n", tptr->tcbName);
    printf("Next instruction adress: %lx\n", getRestartPC(tptr));
    printf("Stack:\n");
    Arch_userStackTrace(tptr);
}

static inline void
debug_printTCB(tcb_t *tcb)
{
    printf("%40s\t", tcb->tcbName);
    char* state;
    switch (thread_state_get_tsType(tcb->tcbState)) {
    case ThreadState_Inactive:
        state = "inactive";
        break;
    case ThreadState_Running:
        state = "running";
        break;
    case ThreadState_Restart:
        state = "restart";
        break;
    case ThreadState_BlockedOnReceive:
        state = "blocked on recv";
        break;
    case ThreadState_BlockedOnSend:
        state = "blocked on send";
        break;
    case ThreadState_BlockedOnReply:
        state = "blocked on reply";
        break;
    case ThreadState_BlockedOnNotification:
        state = "blocked on ntfn";
        break;
#ifdef CONFIG_VTX
    case ThreadState_RunningVM:
        state = "running VM";
        break;
#endif
    case ThreadState_IdleThreadState:
        state = "idle";
        break;
    default:
        fail("Unknown thread state");
    }

    word_t core = SMP_TERNARY(tcb->tcbAffinity, 0);
    printf("%15s\t%p\t%20lu\t%lu\n", state, (void *) getRestartPC(tcb), tcb->tcbPriority, core);
}

static inline void
debug_dumpScheduler(void)
{
    printf("Dumping all tcbs!\n");
    printf("Name                                    \tState          \tIP                  \t Prio \t Core\n");
    printf("--------------------------------------------------------------------------------------\n");
    for (tcb_t *curr = NODE_STATE(ksDebugTCBs); curr != NULL; curr = curr->tcbDebugNext) {
        debug_printTCB(curr);
    }
}
#endif /* CONFIG_PRINTING */
#endif /* __API_DEBUG_H */
#endif /* CONFIG_DEBUG_BUILD */

