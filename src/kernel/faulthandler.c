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
    exception_t status;
    fault_t fault = current_fault;

    status = sendFaultIPC(tptr);
    if (status != EXCEPTION_NONE) {
        handleDoubleFault(tptr, fault);
    }
}

exception_t
sendFaultIPC(tcb_t *tptr)
{
    cptr_t handlerCPtr;
    cap_t  handlerCap;
    lookupCap_ret_t lu_ret;
    lookup_fault_t original_lookup_fault;

    original_lookup_fault = current_lookup_fault;

    handlerCPtr = tptr->tcbFaultHandler;
    lu_ret = lookupCap(tptr, handlerCPtr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_fault = fault_cap_fault_new(handlerCPtr, false);
        return EXCEPTION_FAULT;
    }
    handlerCap = lu_ret.cap;

    if (cap_get_capType(handlerCap) == cap_endpoint_cap &&
            cap_endpoint_cap_get_capCanSend(handlerCap) &&
            cap_endpoint_cap_get_capCanGrant(handlerCap)) {
        tptr->tcbFault = current_fault;
        if (fault_get_faultType(current_fault) == fault_cap_fault) {
            tptr->tcbLookupFailure = original_lookup_fault;
        }
        sendIPC(true, false,
                cap_endpoint_cap_get_capEPBadge(handlerCap),
                true, tptr,
                EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap)));

        return EXCEPTION_NONE;
    } else {
        current_fault = fault_cap_fault_new(handlerCPtr, false);
        current_lookup_fault = lookup_fault_missing_capability_new(0);

        return EXCEPTION_FAULT;
    }
}

#ifdef DEBUG
static void
print_fault(fault_t f)
{
    switch (fault_get_faultType(f)) {
    case fault_null_fault:
        printf("null fault");
        break;
    case fault_cap_fault:
        printf("cap fault in %s phase at address 0x%x",
               fault_cap_fault_get_inReceivePhase(f) ? "receive" : "send",
               (unsigned int)fault_cap_fault_get_address(f));
        break;
    case fault_vm_fault:
        printf("vm fault on %s at address 0x%x with status 0x%x",
               fault_vm_fault_get_instructionFault(f) ? "code" : "data",
               (unsigned int)fault_vm_fault_get_address(f),
               (unsigned int)fault_vm_fault_get_FSR(f));
        break;
    case fault_unknown_syscall:
        printf("unknown syscall 0x%x",
               (unsigned int)fault_unknown_syscall_get_syscallNumber(f));
        break;
    case fault_user_exception:
        printf("user exception 0x%x code 0x%x",
               (unsigned int)fault_user_exception_get_number(f),
               (unsigned int)fault_user_exception_get_code(f));
        break;
    default:
        printf("unknown fault");
        break;
    }
}
#endif

/* The second fault, ex2, is stored in the global current_fault */
void
handleDoubleFault(tcb_t *tptr, fault_t ex1)
{
#ifdef DEBUG
    fault_t ex2 = current_fault;
    printf("Caught ");
    print_fault(ex2);
    printf("\nwhile trying to handle:\n");
    print_fault(ex1);
    printf("\nin thread 0x%x ", (unsigned int)tptr);
    printf("at address 0x%x\n", (unsigned int)getRestartPC(tptr));
#endif

    setThreadState(tptr, ThreadState_Inactive);
}
