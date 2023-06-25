/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once
#include <sel4/config.h>
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
#endif
