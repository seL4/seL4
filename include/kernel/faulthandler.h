/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <object.h>

static bool_t isValidFaultHandler(cap_t cap, bool_t allow_null_cap)
{
    switch (cap_get_capType(cap)) {
    case cap_endpoint_cap:
        return cap_endpoint_cap_get_capCanSend(cap) &&
               (cap_endpoint_cap_get_capCanGrant(cap) ||
                cap_endpoint_cap_get_capCanGrantReply(cap));
    case cap_null_cap:
        return allow_null_cap;
    default:
        break;
    }
    return false;
}

#ifdef CONFIG_KERNEL_MCS
static inline bool_t validTimeoutHandler(tcb_t *tptr)
{
    cap_t handlerCap = TCB_PTR_CTE_PTR(tptr, tcbTimeoutHandler)->cap)
    return isValidFaultHandler(handlerCap, false);
}

void handleTimeout(tcb_t *tptr);
void handleNoFaultHandler(tcb_t *tptr);
bool_t sendFaultIPC(tcb_t *tptr, cap_t handlerCap, bool_t can_donate);
#else
exception_t sendFaultIPC(tcb_t *tptr);
void handleDoubleFault(tcb_t *tptr, seL4_Fault_t ex1);
#endif
void handleFault(tcb_t *tptr);

