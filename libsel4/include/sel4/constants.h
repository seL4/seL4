/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/macros.h>

#ifndef __ASSEMBLER__

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
typedef enum {
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
    seL4_MsgMaxLength = 120,
};
#define seL4_MsgMaxExtraCaps (LIBSEL4_BIT(seL4_MsgExtraCapBits)-1)

/* seL4_CapRights_t defined in shared_types_*.bf */
#define seL4_CapRightsBits 4

typedef enum {
    seL4_NoFailure = 0,
    seL4_InvalidRoot,
    seL4_MissingCapability,
    seL4_DepthMismatch,
    seL4_GuardMismatch,
    SEL4_FORCE_LONG_ENUM(seL4_LookupFailureType),
} seL4_LookupFailureType;
#endif /* !__ASSEMBLER__ */

#ifdef CONFIG_KERNEL_MCS
/* Minimum size of a scheduling context (2^{n} bytes) */
#define seL4_MinSchedContextBits 7
#ifndef __ASSEMBLER__
/* The size of a scheduling context, including the minimum 2 refills, excluding
   any extra refills (= 10 words, 2 tick_t, 2 refills (= 2 tick_t each)) */
#define seL4_CoreSchedContextBytes (10 * sizeof(seL4_Word) + (6 * 8))
/* the size of a single extra refill */
#define seL4_RefillSizeBytes (2 * 8)
SEL4_COMPILE_ASSERT(MinSchedContextBits_min_1, seL4_MinSchedContextBits > 1)
SEL4_COMPILE_ASSERT(MinSchedContextBits_sufficient,
                    seL4_CoreSchedContextBytes <= LIBSEL4_BIT(seL4_MinSchedContextBits))
SEL4_COMPILE_ASSERT(MinSchedContextBits_necessary,
                    seL4_CoreSchedContextBytes > LIBSEL4_BIT(seL4_MinSchedContextBits - 1))

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

/* Flags to be used with seL4_SchedControl_ConfigureFlags */
typedef enum {
    seL4_SchedContext_NoFlag = 0x0,
    seL4_SchedContext_Sporadic = 0x1,
    SEL4_FORCE_LONG_ENUM(seL4_SchedContextFlag),
} seL4_SchedContextFlag;

#endif /* !__ASSEMBLER__ */
#endif /* CONFIG_KERNEL_MCS */

#ifdef CONFIG_KERNEL_INVOCATION_REPORT_ERROR_IPC
#define DEBUG_MESSAGE_START 6
#define DEBUG_MESSAGE_MAXLEN 50
#endif
