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
bool_t tryRaisingTimeoutFault(tcb_t *tptr, word_t scBadge);
#endif

void handleFault(tcb_t *tptr);
