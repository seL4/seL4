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

static bool_t isActiveFaultHander(cap_t handlerCap)
{
    /* validFaultHandler() allows null-caps, but here we really need an entity
     * to be set up that is capable of handling the faults.
     */
    return (cap_get_capType(handlerCap) != cap_null_cap) &&
           validFaultHandler(handlerCap);
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
    if (!isActiveFaultHander(handlerCap)) {
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
    /* set fault details from global variables */
    tptr->tcbFault = current_fault;
    if (seL4_Fault_get_seL4_FaultType(current_fault) == seL4_Fault_CapFault) {
        tptr->tcbLookupFailure = current_lookup_fault;
    }

    /* get fault hander, if it is valid then send fault IPC */
#ifdef CONFIG_KERNEL_MCS
    cap_t handlerCap = TCB_PTR_CTE_PTR(tptr, tcbFaultHandler)->cap;
    if (isActiveFaultHander(handlerCap)) {
        bool_t can_donate = (tptr->tcbSchedContext != NULL);
        sendFaultIPC(tptr, handlerCap, can_donate);
        return;
    }
#else /* not CONFIG_KERNEL_MCS */
    lookupCap_ret_t lu_ret = lookupCap(tptr, tptr->tcbFaultHandler);
    if (lu_ret.status == EXCEPTION_NONE) {
        cap_t handlerCap = lu_ret.cap;
        if (isActiveFaultHander(handlerCap)) {
            sendIPC(true, /* blocking */
                    true, /* can do call */
                    cap_endpoint_cap_get_capEPBadge(handlerCap),
                    cap_endpoint_cap_get_capCanGrant(handlerCap),
                    true, /* canGrantReply */
                    tptr,
                    EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap)));
            return;
        }
    }
#endif /* [not] CONFIG_KERNEL_MCS */

    /* There is no fault handler, log the fault and suspend the thread. */
#ifdef CONFIG_PRINTING
    print_thread_fault(tptr);
#endif /* CONFIG_PRINTING */
    setThreadState(tptr, ThreadState_Inactive);
}
