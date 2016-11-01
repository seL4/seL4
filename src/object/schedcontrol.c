/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */
#include <machine/timer.h>
#include <mode/api/ipc_buffer.h>
#include <object/schedcontext.h>
#include <object/schedcontrol.h>

static exception_t
invokeSchedControl_Configure(sched_context_t *target, ticks_t budget, word_t core)
{
    target->scBudget = budget;
    target->scCore = core;
    recharge(target);

    if (target->scTcb != NULL) {
        /* target may no longer have budget for this core */
        if (!isSchedulable(target->scTcb)) {
            tcbSchedDequeue(target->scTcb);
        } else {
            switchIfRequiredTo(target->scTcb);
        }
    }

    return EXCEPTION_NONE;
}

static exception_t
decodeSchedControl_Configure(word_t length, cap_t cap, extra_caps_t extraCaps, word_t *buffer)
{
    if (extraCaps.excaprefs[0] == NULL) {
        userError("SchedControl_Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < TIME_ARG_SIZE) {
        userError("SchedControl_configure: truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    time_t budget_us = mode_parseTimeArg(0, buffer);

    cap_t targetCap = extraCaps.excaprefs[0]->cap;
    if (unlikely(cap_get_capType(targetCap) != cap_sched_context_cap)) {
        userError("SchedControl_Configure: target cap not a scheduling context cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (budget_us > getMaxTimerUs() || budget_us < getKernelWcetUs()) {
        userError("SchedControl_Configure: budget out of range.");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = getKernelWcetUs();
        current_syscall_error.rangeErrorMax = getMaxTimerUs();
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedControl_Configure(SC_PTR(cap_sched_context_cap_get_capSCPtr(targetCap)),
                                        usToTicks(budget_us),
                                        cap_sched_control_cap_get_core(cap));
}

exception_t
decodeSchedControlInvocation(word_t label, cap_t cap, word_t length, extra_caps_t extraCaps,
                            word_t *buffer)
{
    switch (label) {
    case SchedControlConfigure:
        return  decodeSchedControl_Configure(length, cap, extraCaps, buffer);
    default:
        userError("SchedControl invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}
