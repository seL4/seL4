/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <object.h>

#ifdef CONFIG_KERNEL_MCS
static inline bool_t validTimeoutHandler(tcb_t *tptr)
{
    return cap_get_capType(TCB_PTR_CTE_PTR(tptr, tcbTimeoutHandler)->cap) == cap_endpoint_cap;
}

void handleTimeout(tcb_t *tptr);
void handleNoFaultHandler(tcb_t *tptr);
bool_t sendFaultIPC(tcb_t *tptr, cap_t handlerCap, bool_t can_donate);
#else
exception_t sendFaultIPC(tcb_t *tptr);
void handleDoubleFault(tcb_t *tptr, seL4_Fault_t ex1);
#endif
void handleFault(tcb_t *tptr);

