/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <util.h>
#include <api/failures.h>
#include <api/debug.h>
#include <kernel/cspace.h>
#include <kernel/faulthandler.h>
#include <kernel/thread.h>
#include <arch/machine.h>

static cap_t getFaultHandlerCap(tcb_t *tptr)
{
#ifdef CONFIG_KERNEL_MCS
    return TCB_PTR_CTE_PTR(tptr, tcbFaultHandler)->cap;
#else
    lookupCap_ret_t lu_ret = lookupCap(tptr, tptr->tcbFaultHandler);
    return (lu_ret.status == EXCEPTION_NONE) ? lu_ret.cap : cap_null_cap_new();
#endif
}

#ifdef CONFIG_KERNEL_MCS

static void sendFaultIPC(tcb_t *tptr, cap_t handlerCap, bool_t can_donate)
{
    sendIPC(true, /* blocking */
            false, /* can't do call */
            cap_endpoint_cap_get_capEPBadge(handlerCap),
            cap_endpoint_cap_get_capCanGrant(handlerCap),
            cap_endpoint_cap_get_capCanGrantReply(handlerCap),
            can_donate,
            tptr,
            EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap)));
}

bool_t tryRaisingTimeoutFault(tcb_t *tptr, word_t scBadge)
{
    /* get the timeout handler */
    cap_t handlerCap = TCB_PTR_CTE_PTR(tptr, tcbTimeoutHandler)->cap;
    if (!isValidFaultHandler(handlerCap, false)) {
        return false;
    }

    /* create the time fault and send the fault IPC */
    tptr->tcbFault = seL4_Fault_Timeout_new(scBadge);
    sendFaultIPC(tptr, handlerCap, false);

    return true;
}

#endif /* CONFIG_KERNEL_MCS */

void handleFault(tcb_t *tptr)
{
    /* Set fault details from global variables. */
    tptr->tcbFault = current_fault;
    if (seL4_Fault_get_seL4_FaultType(current_fault) == seL4_Fault_CapFault) {
        tptr->tcbLookupFailure = current_lookup_fault;
    }

    /*
     * Check if there is a fault handler. If not then log the fault details and
     * suspend the thread.
     */
    cap_t handlerCap = getFaultHandlerCap(tptr);
    if (!isValidFaultHandler(handlerCap, false)) {
#ifdef CONFIG_PRINTING
        print_thread_fault(tptr);
#endif /* CONFIG_PRINTING */
        setThreadState(tptr, ThreadState_Inactive);
        return;
    }

    /* Send fault to the hander */
#ifdef CONFIG_KERNEL_MCS
    bool_t can_donate = (tptr->tcbSchedContext != NULL);
    sendFaultIPC(tptr, handlerCap, can_donate);
#else /* not CONFIG_KERNEL_MCS */
    sendIPC(true, /* blocking */
            true, /* can do call */
            cap_endpoint_cap_get_capEPBadge(handlerCap),
            cap_endpoint_cap_get_capCanGrant(handlerCap),
            true, /* canGrantReply */
            tptr,
            EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap)));
#endif /* [not] CONFIG_KERNEL_MCS */

}
