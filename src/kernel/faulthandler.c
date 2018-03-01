/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <api/failures.h>
#include <kernel/cspace.h>
#include <kernel/faulthandler.h>
#include <kernel/thread.h>
#include <machine/io.h>
#include <arch/machine.h>

void
handleFault(tcb_t *tptr)
{
    bool_t hasFaultHandler = sendFaultIPC(tptr, TCB_PTR_CTE_PTR(tptr, tcbFaultHandler)->cap, true);
    if (!hasFaultHandler) {
        handleNoFaultHandler(tptr);
    }
}

void
handleTimeout(tcb_t *tptr)
{
    assert(validTimeoutHandler(tptr));
    sendFaultIPC(tptr, TCB_PTR_CTE_PTR(tptr, tcbTimeoutHandler)->cap, false);
}

bool_t
sendFaultIPC(tcb_t *tptr, cap_t handlerCap, bool_t can_donate)
{
    if (cap_get_capType(handlerCap) == cap_endpoint_cap) {
        assert(cap_endpoint_cap_get_capCanSend(handlerCap));
        assert(cap_endpoint_cap_get_capCanGrant(handlerCap));

        tptr->tcbFault = current_fault;
        sendIPC(true, false,
                cap_endpoint_cap_get_capEPBadge(handlerCap),
                true, can_donate, tptr,
                EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap)));

        return true;
    } else {
        assert(cap_get_capType(handlerCap) == cap_null_cap);
        return false;
    }
}

#ifdef CONFIG_PRINTING
static void
print_fault(seL4_Fault_t f)
{
    switch (seL4_Fault_get_seL4_FaultType(f)) {
    case seL4_Fault_NullFault:
        printf("null fault");
        break;
    case seL4_Fault_CapFault:
        printf("cap fault in %s phase at address 0x%x",
               seL4_Fault_CapFault_get_inReceivePhase(f) ? "receive" : "send",
               (unsigned int)seL4_Fault_CapFault_get_address(f));
        break;
    case seL4_Fault_VMFault:
        printf("vm fault on %s at address 0x%x with status 0x%x",
               seL4_Fault_VMFault_get_instructionFault(f) ? "code" : "data",
               (unsigned int)seL4_Fault_VMFault_get_address(f),
               (unsigned int)seL4_Fault_VMFault_get_FSR(f));
        break;
    case seL4_Fault_UnknownSyscall:
        printf("unknown syscall 0x%x",
               (unsigned int)seL4_Fault_UnknownSyscall_get_syscallNumber(f));
        break;
    case seL4_Fault_UserException:
        printf("user exception 0x%x code 0x%x",
               (unsigned int)seL4_Fault_UserException_get_number(f),
               (unsigned int)seL4_Fault_UserException_get_code(f));
        break;
    case seL4_Fault_Timeout:
        printf("Timeout fault for 0x%x\n", (unsigned int) seL4_Fault_Timeout_get_badge(f));
        break;
    default:
        printf("unknown fault");
        break;
    }
}
#endif

void
handleNoFaultHandler(tcb_t *tptr)
{
#ifdef CONFIG_PRINTING
    printf("Found thread has no fault handler while trying to handle:\n");
    print_fault(current_fault);

#ifdef CONFIG_DEBUG_BUILD
    printf("\nin thread %p \"%s\" ", tptr, tptr->tcbName);
#endif /* CONFIG_DEBUG_BUILD */

    printf("at address %p\n", (void*)getRestartPC(tptr));
    printf("With stack:\n");
    Arch_userStackTrace(tptr);
#endif

    setThreadState(tptr, ThreadState_Inactive);
}
