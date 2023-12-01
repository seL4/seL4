/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2022, Axel Heider <axelheider@gmx.de>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_PRINTING

#include <types.h>
#include <util.h>
#include <api/debug.h>
#include <machine/io.h>
#include <kernel/thread.h>


/* This function is also available in release builds when printing is enabled,
 * in this case it will not print any register content to avoid leaking
 * potentially sensitive data.
 */
void print_thread_fault(tcb_t *tptr)
{
    const char *name = config_ternary(CONFIG_DEBUG_BUILD,
                                      TCB_PTR_DEBUG_PTR(tptr)->tcbName,
                                      null);

    printf("FAULT at address %p in thread %p%s%s%s\n"
           "      ",
           (void *)getRestartPC(tptr),
           tptr,
           name ? " (" : "",
           name,
           name ? ")" : "");

    seL4_Fault_t fault = tptr->tcbFault;
    word_t fault_type = seL4_Fault_get_seL4_FaultType(fault);

    switch (fault_type) {
    case seL4_Fault_NullFault:
        printf("null fault");
        break;
    case seL4_Fault_CapFault:
        printf("cap fault in %s phase at address %p",
               seL4_Fault_CapFault_get_inReceivePhase(fault) ? "receive" : "send",
               (void *)seL4_Fault_CapFault_get_address(fault));
        /* ToDo: show tptr->tcbLookupFailure also */
        break;
    case seL4_Fault_VMFault:
        printf("vm fault on %s at address %p with status %p",
               seL4_Fault_VMFault_get_instructionFault(fault) ? "code" : "data",
               (void *)seL4_Fault_VMFault_get_address(fault),
               (void *)seL4_Fault_VMFault_get_FSR(fault));
        break;
    case seL4_Fault_UnknownSyscall:
        printf("unknown syscall %p",
               (void *)seL4_Fault_UnknownSyscall_get_syscallNumber(fault));
        break;
    case seL4_Fault_UserException:
        printf("user exception %p code %p",
               (void *)seL4_Fault_UserException_get_number(fault),
               (void *)seL4_Fault_UserException_get_code(fault));
        break;
#ifdef CONFIG_KERNEL_MCS
    case seL4_Fault_Timeout:
        printf("Timeout fault for badge %p",
               (void *)seL4_Fault_Timeout_get_badge(fault));
        break;
#endif
    default:
        printf("unknown type %"SEL4_PRIu_word, fault_type);
        break;
    }

    printf("\nStack dump:\n");
    Arch_userStackTrace(tptr);
}

#ifdef CONFIG_DEBUG_BUILD

void debug_printKernelEntryReason(void)
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

void debug_printUserState(void)
{
    tcb_t *tptr = NODE_STATE(ksCurThread);
    printf("Current thread: %s\n", TCB_PTR_DEBUG_PTR(tptr)->tcbName);
    printf("Next instruction adress: %lx\n", getRestartPC(tptr));
    printf("Stack:\n");
    Arch_userStackTrace(tptr);
}

void debug_printTCB(tcb_t *tcb)
{
    printf("%40s\t", TCB_PTR_DEBUG_PTR(tcb)->tcbName);
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

    word_t core = SMP_TERNARY(tcb->tcbAffinity, 0);
    printf("%15s\t%p\t%20lu\t%lu", state, (void *) getRestartPC(tcb), tcb->tcbPriority, core);
#ifdef CONFIG_KERNEL_MCS
    printf("\t%lu", (word_t) thread_state_get_tcbInReleaseQueue(tcb->tcbState));
#endif
    printf("\n");
}

void debug_dumpScheduler(void)
{
    printf("Dumping all tcbs!\n");
    printf("Name                                    \tState          \tIP                  \t Prio \t Core%s\n",
           config_set(CONFIG_KERNEL_MCS) ?  "\t InReleaseQueue" : "");
    printf("--------------------------------------------------------------------------------------\n");
    for (tcb_t *curr = NODE_STATE(ksDebugTCBs); curr != NULL; curr = TCB_PTR_DEBUG_PTR(curr)->tcbDebugNext) {
        debug_printTCB(curr);
    }
}

#endif /* CONFIG_DEBUG_BUILD */
#endif /* CONFIG_PRINTING */
