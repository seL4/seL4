/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once
#include <autoconf.h>
#include <sel4/types.h>
#include <sel4/macros.h>
#include <sel4/invocation.h>
#include <sel4/constants.h>
#include <interfaces/sel4_client.h>

/*
 * This file specifies virtual implementations of older invocations
 * that can be aliased directly to new invocations.
 */

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE seL4_Error seL4_SchedControl_Configure(seL4_SchedControl _service, seL4_SchedContext schedcontext,
                                                      seL4_Time budget, seL4_Time period, seL4_Word extra_refills, seL4_Word badge)
{
    return seL4_SchedControl_ConfigureFlags(_service, schedcontext, budget, period, extra_refills, badge,
                                            seL4_SchedContext_NoFlag);
}

LIBSEL4_INLINE seL4_Error seL4_TCB_SetSchedParams(seL4_TCB _service, seL4_TCB authority, seL4_Word mcp,
                                                  seL4_Word priority, seL4_CPtr sched_context, seL4_CPtr fault_ep)
{
    return seL4_TCB_SetSchedParamsFH(_service, authority, mcp, priority, sched_context, fault_ep, 0, seL4_NoRights);
}

LIBSEL4_INLINE seL4_Error seL4_TCB_SetTimeoutEndpoint(seL4_TCB _service, seL4_CPtr timeout_fault_ep)
{
    return seL4_TCB_SetTimeoutEndpointBadge(_service, timeout_fault_ep, 0, seL4_NoRights);
}

LIBSEL4_INLINE seL4_Error seL4_TCB_SetSpace(seL4_TCB _service, seL4_Word fault_ep, seL4_CNode cspace_root,
                                            seL4_Word cspace_root_data, seL4_CPtr vspace_root, seL4_Word vspace_root_data)
{
    return seL4_TCB_SetSpaceFH(_service, fault_ep, cspace_root, cspace_root_data, vspace_root, vspace_root_data, 0,
                               seL4_NoRights);
}
#endif
