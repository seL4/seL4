/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_FAULTHANDLER_H
#define __KERNEL_FAULTHANDLER_H

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

#endif
