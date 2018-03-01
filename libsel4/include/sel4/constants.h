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

#ifndef __API_CONSTANTS_H
#define __API_CONSTANTS_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#define LIBSEL4_BIT(n) (1ul<<(n))

#ifdef CONFIG_HARDWARE_DEBUG_API
/* API arg values for breakpoint API, "type" arguments. */
typedef enum {
    seL4_DataBreakpoint = 0,
    seL4_InstructionBreakpoint,
    seL4_SingleStep,
    seL4_SoftwareBreakRequest,
    SEL4_FORCE_LONG_ENUM(seL4_BreakpointType)
} seL4_BreakpointType;

/* API arg values for breakpoint API, "access" arguments. */
typedef enum {
    seL4_BreakOnRead = 0,
    seL4_BreakOnWrite,
    seL4_BreakOnReadWrite,
    seL4_MaxBreakpointAccess,
    SEL4_FORCE_LONG_ENUM(seL4_BreakpointAccess)
} seL4_BreakpointAccess;

/* Format of a debug-exception message. */
enum {
    seL4_DebugException_FaultIP,
    seL4_DebugException_ExceptionReason,
    seL4_DebugException_TriggerAddress,
    seL4_DebugException_BreakpointNumber,
    seL4_DebugException_Length,
    SEL4_FORCE_LONG_ENUM(seL4_DebugException_Msg)
} seL4_DebugException_Msg;
#endif

enum priorityConstants {
    seL4_InvalidPrio = -1,
    seL4_MinPrio = 0,
    seL4_MaxPrio = CONFIG_NUM_PRIORITIES - 1
};

/* seL4_MessageInfo_t defined in api/shared_types.bf */

enum seL4_MsgLimits {
    seL4_MsgLengthBits = 7,
    seL4_MsgExtraCapBits = 2
};

enum {
    seL4_MsgMaxLength = 119,
};
#define seL4_MsgMaxExtraCaps (LIBSEL4_BIT(seL4_MsgExtraCapBits)-1)

typedef enum {
    seL4_NoFailure = 0,
    seL4_InvalidRoot,
    seL4_MissingCapability,
    seL4_DepthMismatch,
    seL4_GuardMismatch,
    SEL4_FORCE_LONG_ENUM(seL4_LookupFailureType),
} seL4_LookupFailureType;

/* Minimum size of a scheduling context (2^{n} bytes) */
#define seL4_MinSchedContextBits 8
/* the size of a scheduling context, excluding extra refills */
#define seL4_CoreSchedContextBytes (8 * sizeof(seL4_Word) + (6 * 8))
/* the size of a single extra refill */
#define seL4_RefillSizeBytes (2 * 8)

/*
 * @brief Calculate the max extra refills a scheduling context can contain for a specific size.
 *
 * @param  size of the schedulding context. Must be >= seL4_MinSchedContextBits
 * @return the max number of extra refills that can be passed to seL4_SchedControl_Configure for
 *         this scheduling context
 */
static inline seL4_Word seL4_MaxExtraRefills(seL4_Word size)
{
    return (LIBSEL4_BIT(size) -  seL4_CoreSchedContextBytes) / seL4_RefillSizeBytes;
}

#endif /* __API_CONSTANTS_H */
