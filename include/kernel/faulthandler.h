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

static inline cap_t
getTemporalFaultHandler(tcb_t *tcb)
{
    return TCB_PTR_CTE_PTR(tcb, tcbTemporalFaultHandler)->cap;
}

static inline bool_t
validTemporalFaultHandler(cap_t cap)
{
    return cap_get_capType(cap) == cap_endpoint_cap;
}

void handleFault(tcb_t *tptr);
exception_t sendFaultIPC(tcb_t *tptr);
exception_t sendTemporalFaultIPC(tcb_t *tptr, cap_t tfep);
void handleDoubleFault(tcb_t *tptr, fault_t ex1);

#endif
