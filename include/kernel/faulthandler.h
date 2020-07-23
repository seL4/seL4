/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <object.h>
#ifdef CONFIG_KERNEL_MCS
#include <kernel/sporadic.h>
#endif

#ifdef CONFIG_KERNEL_MCS
static inline bool_t validTimeoutHandler(tcb_t *tptr)
{
    cap_t handlerCap = TCB_PTR_CTE_PTR(tptr, tcbTimeoutHandler)->cap;
    if (likely(cap_get_capType(handlerCap) == cap_endpoint_cap)) {
        endpoint_t *handler = EP_PTR(cap_endpoint_cap_get_capEPPtr(handlerCap));
        if (endpoint_ptr_get_state(handler) == EPState_Recv) {
            tcb_queue_t queue = ep_ptr_get_queue(handler);
            tcb_t *dest = queue.head;
            /* If the recevier does not have its own valid SC, the
             * timeout handler is not valid */
            sched_context_t *sc = dest->tcbSchedContext;
            return sc != NULL && sc->scRefillMax > 0 && refill_ready(sc);
        } else {
            return true;
        }
    } else {
        return false;
    }
}

void handleTimeout(tcb_t *tptr);
void handleNoFaultHandler(tcb_t *tptr);
bool_t sendFaultIPC(tcb_t *tptr, cap_t handlerCap, bool_t can_donate);
#else
exception_t sendFaultIPC(tcb_t *tptr);
void handleDoubleFault(tcb_t *tptr, seL4_Fault_t ex1);
#endif
void handleFault(tcb_t *tptr);

