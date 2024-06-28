/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <mode/object/structures.h>

#define tcbArchCNodeEntries tcbCNodeEntries

static inline bool_t CONST Arch_isCapRevocable(cap_t derivedCap, cap_t srcCap)
{
    switch (cap_get_capType(derivedCap)) {

#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        return cap_get_capType(srcCap) == cap_irq_control_cap;
#endif

#ifdef CONFIG_ALLOW_SMC_CALLS
    case cap_smc_cap:
        return (cap_smc_cap_get_capSMCBadge(derivedCap) !=
                cap_smc_cap_get_capSMCBadge(srcCap));
#endif

    default:
        return false;
    }
}
