/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <arch/api/ipc_buffer.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <machine/registerset.h>
#include <model/statedata.h>
#include <object/notification.h>
#include <object/cnode.h>
#include <object/endpoint.h>
#include <object/tcb.h>

#include <object/schedcontext.h>

exception_t
invokeSchedControl_Configure(sched_context_t *target, time_t budget, time_t period)
{
    budget = usToTicks(budget);
    period = usToTicks(period);

    target->budget = budget;
    target->period = period;

    recharge(target);

    return EXCEPTION_NONE;
}

static bool_t
validTemporalParam(time_t param)
{

    if (param > getMaxTimerUs()) {
        userError("SchedControl_Configure: param too large, max for this platform %llu.", getMaxTimerUs());
        return false;
    }

    if (param < getKernelWcetUs()) {
        userError("SchedControl_Configure: param too small, min for this platform %llx (got %llx).",
                  getKernelWcetUs(), param);
        return false;
    }

    return true;
}

exception_t
decodeSchedControl_Configure(word_t length, extra_caps_t extra_caps, word_t *buffer)
{
    parseTime_ret_t ret;
    time_t budget, period;
    cap_t targetCap;
    sched_context_t *target;

    if (unlikely(length < 2 || extra_caps.excaprefs[0] == NULL)) {
        userError("SchedControl_Configure: truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    ret = arch_parseTimeArg(0, buffer);
    budget = ret.arg;
    ret = arch_parseTimeArg(ret.words, buffer);
    period = ret.arg;

    if (unlikely(!validTemporalParam(budget))) {
        userError("Budget invalid");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(!validTemporalParam(period))) {
        userError("Period invalid");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 2;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (budget > period) {
        userError("SchedControl_Configure: budget cannot be greater than period");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    targetCap = extra_caps.excaprefs[0]->cap;

    if (unlikely(cap_get_capType(targetCap) != cap_sched_context_cap)) {
        userError("SchedControl_Configure: target cap not a scheduling context cap.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    target = SC_PTR(cap_sched_context_cap_get_capPtr(targetCap));
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedControl_Configure(target, budget, period);
}

exception_t
decodeSchedControlInvocation(word_t label, word_t length, extra_caps_t extraCaps, word_t *buffer)
{
    switch (label) {
    case SchedControlConfigure:
        return decodeSchedControl_Configure(length, extraCaps, buffer);
    default:
        userError("SchedControl invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

