/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */
#pragma once

#include <autoconf.h>
#include <sel4/types.h>
#include <sel4/sel4_arch/faults.h>

LIBSEL4_INLINE_FUNC seL4_Fault_t
seL4_getFault(seL4_MessageInfo_t tag)
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
LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isDebugException_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_DebugException;
}
#endif

LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isVMFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_VMFault;
}

LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isUnknownSyscall_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_UnknownSyscall;
}

LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isUserException_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_UserException;
}

LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isNullFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_NullFault;
}

LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isCapFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_CapFault;
}
