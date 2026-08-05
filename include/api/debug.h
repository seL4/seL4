/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_DEBUG_BUILD
#pragma once

#include <benchmark/benchmark_track.h>
#include <arch/api/syscall.h>
#include <arch/kernel/vspace.h>
#include <model/statedata.h>
#include <kernel/thread.h>

#ifdef CONFIG_PRINTING

static inline void debug_printKernelEntryReason(void)
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
        printf("Unknown (%u)\n", ksKernelEntry.path);
        break;

    }
}

/* Prints the user context and stack trace of the current thread */
static inline void debug_printUserState(void)
{
    tcb_t *tptr = NODE_STATE(ksCurThread);
    printf("Current thread: %s\n", TCB_PTR_DEBUG_PTR(tptr)->tcbName);
    printf("Next instruction address: %lx\n", getRestartPC(tptr));
    printf("Stack:\n");
    Arch_userStackTrace(tptr);
}

static inline void debug_printTCB(tcb_t *tcb)
{
    char *state;
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

    /* 40: (arbitrary) max print-length of name
     * 16: max length of state
     * 18: length of `0x + max 64 bit number'
     * 4:  length of "Prio" header
     * 4:  length of "Core" header
     * 4:  length of "Dom" header, but +1 for consistency with Prio/Core headers
     * 14: length of "InReleaseQueue" header */
    printf("%-40s   %-16s   %-18p   %4lu", TCB_PTR_DEBUG_PTR(tcb)->tcbName, state,
           (void *) getRestartPC(tcb), tcb->tcbPriority);
#ifdef CONFIG_ENABLE_SMP_SUPPORT
    printf("   %4lu", tcb->tcbAffinity);
#endif
#if CONFIG_NUM_DOMAINS > 1
    printf("   %4lu", tcb->tcbDomain);
#endif
#ifdef CONFIG_KERNEL_MCS
    printf("   %-14s", thread_state_get_tcbInReleaseQueue(tcb->tcbState) ? "yes" : "no");
#endif
    printf("\n");
}

static inline void debug_dumpScheduler(void)
{
    printf("Dumping all tcbs!\n");
    /* keep in sync with debug_printTCB */
    printf("%-40s   %-16s   %-18s   %-4s", "Name", "State", "IP", "Prio");
#ifdef CONFIG_ENABLE_SMP_SUPPORT
    printf("   %4s", "Core");
#endif
#if CONFIG_NUM_DOMAINS > 1
    printf("   %4s", "Dom");
#endif
#ifdef CONFIG_KERNEL_MCS
    printf("   %-14s", "InReleaseQueue");
#endif
    printf("\n");

    /* unfortunately the number of '-' here needs to be manually kept in sync with above */
    printf("----------------------------------------------------------------------------------------%s%s%s\n",
           config_set(CONFIG_ENABLE_SMP_SUPPORT) ? "-------" : "",
           (CONFIG_NUM_DOMAINS > 1)              ? "-------" : "",
           config_set(CONFIG_KERNEL_MCS)         ? "-----------------" : "");
    for (word_t core = 0; core < CONFIG_MAX_NUM_NODES; core++) {
        for (tcb_t *curr = NODE_STATE_ON_CORE(ksDebugTCBs, core); curr != NULL; curr = TCB_PTR_DEBUG_PTR(curr)->tcbDebugNext) {
            debug_printTCB(curr);
        }
    }
}
#endif /* CONFIG_PRINTING */
#endif /* CONFIG_DEBUG_BUILD */

