/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <api/failures.h>
#include <api/debug.h>
#include <kernel/cspace.h>
#include <kernel/faulthandler.h>
#include <kernel/thread.h>
#include <arch/machine.h>

#ifdef CONFIG_KERNEL_MCS

void handleFault(tcb_t *tptr)
{
    bool_t hasFaultHandler = sendFaultIPC(tptr, TCB_PTR_CTE_PTR(tptr, tcbFaultHandler)->cap,
                                          tptr->tcbSchedContext != NULL);
    if (!hasFaultHandler) {
#ifdef CONFIG_PRINTING
        debug_print_fault_handler(tptr, current_fault);
#endif /* CONFIG_PRINTING */
        handleNoFaultHandler(tptr);
    }
}

void handleTimeout(tcb_t *tptr)
{
    assert(validTimeoutHandler(tptr));
    sendFaultIPC(tptr, TCB_PTR_CTE_PTR(tptr, tcbTimeoutHandler)->cap, false);
}

bool_t sendFaultIPC(tcb_t *tptr, cap_t handlerCap, bool_t can_donate)
{
    if (cap_get_capType(handlerCap) == cap_endpoint_cap) {
        assert(cap_endpoint_cap_get_capCanSend(handlerCap));
        assert(cap_endpoint_cap_get_capCanGrant(handlerCap) ||
               cap_endpoint_cap_get_capCanGrantReply(handlerCap));

        tptr->tcbFault = current_fault;
        sendIPC(true, false,
                cap_endpoint_cap_get_capEPBadge(handlerCap),
                cap_endpoint_cap_get_capCanGrant(handlerCap),
                cap_endpoint_cap_get_capCanGrantReply(handlerCap),
                can_donate, tptr,
                EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap)));

        return true;
    } else {
        assert(cap_get_capType(handlerCap) == cap_null_cap);
        return false;
    }
}

void handleNoFaultHandler(tcb_t *tptr)
{
    /* Debug output is done where this is called. */
    setThreadState(tptr, ThreadState_Inactive);
}

#else /* not CONFIG_KERNEL_MCS */

void handleFault(tcb_t *tptr)
{
    seL4_Fault_t fault = current_fault;
    exception_t status = sendFaultIPC(tptr);
    if (status != EXCEPTION_NONE) {
#ifdef CONFIG_PRINTING
        /* current_fault holds the fault from the failed sendFaultIPC() now. */
        debug_print_fault_handler(tptr, fault, status, current_fault);
#endif /* CONFIG_PRINTING */
        handleDoubleFault(tptr, fault);
    }
}

exception_t sendFaultIPC(tcb_t *tptr)
{
    cptr_t handlerCPtr;
    cap_t  handlerCap;
    lookupCap_ret_t lu_ret;
    lookup_fault_t original_lookup_fault;

    original_lookup_fault = current_lookup_fault;

    handlerCPtr = tptr->tcbFaultHandler;
    lu_ret = lookupCap(tptr, handlerCPtr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_fault = seL4_Fault_CapFault_new(handlerCPtr, false);
        return EXCEPTION_FAULT;
    }
    handlerCap = lu_ret.cap;

    if (cap_get_capType(handlerCap) == cap_endpoint_cap &&
        cap_endpoint_cap_get_capCanSend(handlerCap) &&
        (cap_endpoint_cap_get_capCanGrant(handlerCap) ||
         cap_endpoint_cap_get_capCanGrantReply(handlerCap))) {
        tptr->tcbFault = current_fault;
        if (seL4_Fault_get_seL4_FaultType(current_fault) == seL4_Fault_CapFault) {
            tptr->tcbLookupFailure = original_lookup_fault;
        }
        sendIPC(true, true,
                cap_endpoint_cap_get_capEPBadge(handlerCap),
                cap_endpoint_cap_get_capCanGrant(handlerCap), true, tptr,
                EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap)));

        return EXCEPTION_NONE;
    } else {
        current_fault = seL4_Fault_CapFault_new(handlerCPtr, false);
        current_lookup_fault = lookup_fault_missing_capability_new(0);

        return EXCEPTION_FAULT;
    }
}

void handleDoubleFault(tcb_t *tptr, seL4_Fault_t ex1)
{
    /* debug output is done where this is called. */
    setThreadState(tptr, ThreadState_Inactive);
}

#endif /* [not] CONFIG_KERNEL_MCS */
