/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/types.h>
#include <sel4/sel4_arch/faults.h>

LIBSEL4_INLINE_FUNC seL4_Fault_t seL4_getFault(seL4_MessageInfo_t tag)
{

    switch (seL4_MessageInfo_get_label(tag)) {
    case seL4_Fault_CapFault:
        return seL4_Fault_CapFault_new(seL4_GetMR(seL4_CapFault_IP),
                                       seL4_GetMR(seL4_CapFault_Addr),
                                       seL4_GetMR(seL4_CapFault_InRecvPhase),
                                       seL4_GetMR(seL4_CapFault_LookupFailureType),
                                       seL4_GetMR(seL4_CapFault_BitsLeft),
                                       seL4_GetMR(seL4_CapFault_GuardMismatch_GuardFound),
                                       seL4_GetMR(seL4_CapFault_GuardMismatch_BitsFound));
#ifdef CONFIG_HARDWARE_DEBUG_API
    case seL4_Fault_DebugException:
        return seL4_Fault_DebugException_new(seL4_GetMR(seL4_DebugException_FaultIP),
                                             seL4_GetMR(seL4_DebugException_ExceptionReason),
                                             seL4_GetMR(seL4_DebugException_TriggerAddress),
                                             seL4_GetMR(seL4_DebugException_BreakpointNumber));
#endif
    default:
        return seL4_getArchFault(tag);
    }
}

#ifdef CONFIG_HARDWARE_DEBUG_API
LIBSEL4_INLINE_FUNC seL4_Bool seL4_isDebugException_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_DebugException;
}
#endif

LIBSEL4_INLINE_FUNC seL4_Bool seL4_isVMFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_VMFault;
}

LIBSEL4_INLINE_FUNC seL4_Bool seL4_isUnknownSyscall_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_UnknownSyscall;
}

LIBSEL4_INLINE_FUNC seL4_Bool seL4_isUserException_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_UserException;
}

LIBSEL4_INLINE_FUNC seL4_Bool seL4_isNullFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_NullFault;
}

LIBSEL4_INLINE_FUNC seL4_Bool seL4_isCapFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_CapFault;
}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_Bool seL4_isTimeoutFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_Timeout;
}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_TimeoutReply_new(seL4_Bool resume, seL4_UserContext regs, seL4_Word length)
{
    seL4_MessageInfo_t info = seL4_MessageInfo_new(!resume, 0, 0, length);
    for (seL4_Word i = 0; i < length; i++) {
        seL4_SetMR(i, ((seL4_Word *) &regs)[i]);
    }

    return info;
}
#endif /* CONFIG_KERNEL_MCS */
