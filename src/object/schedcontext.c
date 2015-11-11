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
invokeSchedControl_Configure(sched_context_t *target, time_t budget)
{
    /* if the target has an active budget, extend it by the difference
      between the old budget and the new */
    if (target->tcb) {
        if (budget > target->budget) {
            target->remaining += (budget - target->budget);
        }
        /* reduced budgets will take effect next time */
    }

    target->budget = budget;

    return EXCEPTION_NONE;
}

exception_t
decodeSchedControl_Configure(word_t length, extra_caps_t extra_caps, word_t *buffer)
{
    time_t budget;
    cap_t targetCap;
    sched_context_t *target;

    if (unlikely(length < 1 || extra_caps.excaprefs[0] == NULL)) {
        userError("SchedControl_Configure: truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    budget = arch_parseTimeArg(1, buffer).arg;

    if (budget == 0llu) {
        userError("SchedControl_Configure: budget too small.");
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
    return invokeSchedControl_Configure(target, budget);
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

