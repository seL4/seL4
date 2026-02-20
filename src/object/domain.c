/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2025, Indan Zupancic
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <util.h>
#include <api/syscall.h>
#include <kernel/thread.h>
#include <mode/api/ipc_buffer.h>
#include <model/statedata.h>
#include <object/domain.h>
#include <object/structures.h>

/* Domain schedules. The duration is in kernel ticks for non-MCS and timer ticks for MCS. */
dschedule_t ksDomSchedule[CONFIG_NUM_DOMAIN_SCHEDULES];

static void invokeDomainSetSet(tcb_t *tcb, dom_t domain)
{
    prepareSetDomain(tcb, domain);
    setDomain(tcb, domain);
}

static exception_t decodeDomainSetSetInvocation(word_t length, word_t *buffer)
{
    dom_t domain;
    cap_t tcap;

    if (unlikely(length == 0)) {
        userError("Domain Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    } else {
        domain = getSyscallArg(0, buffer);
        if (domain >= numDomains) {
            userError("Domain Configure: invalid domain (%lu >= %u).",
                      domain, numDomains);
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }
    }

    if (unlikely(current_extra_caps.excaprefs[0] == NULL)) {
        userError("Domain Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    tcap = current_extra_caps.excaprefs[0]->cap;
    if (unlikely(cap_get_capType(tcap) != cap_thread_cap)) {
        userError("Domain Configure: thread cap required.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    invokeDomainSetSet(TCB_PTR(cap_thread_cap_get_capTCBPtr(tcap)), domain);
    return EXCEPTION_NONE;
}

static void invokeDomainScheduleConfigure(word_t index, dom_t domain, ticks_t duration)
{
    ksDomSchedule[index] = dschedule_make(domain, duration);
}

static exception_t decodeDomainScheduleConfigure(word_t length, word_t *buffer)
{
    word_t index;
    dom_t domain;
    ticks_t duration;

    if (unlikely(length < 2 + TIME_ARG_SIZE)) {
        userError("Domain Schedule Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    index = getSyscallArg(0, buffer);
    domain = getSyscallArg(1, buffer);
    duration = mode_parseTimeArg(2, buffer);

    if (index >= domScheduleLength) {
        userError("Domain Schedule Configure: Invalid index.");
        current_syscall_error.invalidArgumentNumber = 0;
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = domScheduleLength - 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (domain >= numDomains) {
        userError("Domain Schedule Configure: Invalid domain.");
        current_syscall_error.invalidArgumentNumber = 1;
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = numDomains - 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (duration > DSCHED_MAX_DURATION) {
        userError("Domain Schedule Configure: duration must fit in 56 bits.");
        current_syscall_error.invalidArgumentNumber = 2;
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (duration == 0 && domain != 0) {
        userError("Domain Schedule Configure: Both domain and duration must be zero for end markers.");
        current_syscall_error.invalidArgumentNumber = 1;
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (index == ksDomScheduleStart && duration == 0) {
        userError("Domain Schedule Configure: Starting schedule's duration must not be zero.");
        current_syscall_error.invalidArgumentNumber = 2;
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    invokeDomainScheduleConfigure(index, domain, duration);
    return EXCEPTION_NONE;
}

static void invokeDomainScheduleSetStart(word_t index)
{
    ksDomScheduleStart = index;
    /* End the current domain schedule and force an
     * immediate switch to the new starting index: */
    ksDomainTime = 0;
    rescheduleRequired();
}

static exception_t decodeDomainScheduleSetStart(word_t length, word_t *buffer)
{
    word_t index;

    if (unlikely(length < 1)) {
        userError("Domain Schedule Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    index = getSyscallArg(0, buffer);

    if (index >= domScheduleLength) {
        userError("Domain Schedule Set Start: Invalid index.");
        current_syscall_error.invalidArgumentNumber = 0;
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = domScheduleLength - 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (dschedule_is_end_marker(index)) {
        userError("Domain Schedule Set Start: Starting schedule must not be an end marker.");
        current_syscall_error.invalidArgumentNumber = 0;
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    invokeDomainScheduleSetStart(index);
    return EXCEPTION_NONE;
}

exception_t decodeDomainInvocation(word_t invLabel, word_t length, word_t *buffer)
{
    switch (invLabel) {
    case DomainSetSet:
        return decodeDomainSetSetInvocation(length, buffer);
    case DomainScheduleConfigure:
        return decodeDomainScheduleConfigure(length, buffer);
    case DomainScheduleSetStart:
        return decodeDomainScheduleSetStart(length, buffer);
    default:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}
