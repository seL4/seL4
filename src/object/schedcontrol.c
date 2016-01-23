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

#include <object/schedcontrol.h>

static inline bool_t
updateActivePeriod(sched_context_t *sc, ticks_t period)
{
    bool_t release = false;
    /* update the active period (i.e when the thread is due to be released next) */
    /* if the thread is in the release queue we will need to reenqueue it at the end
     * of the update */
    release = thread_state_get_tcbInReleaseQueue(sc->scTcb->tcbState);
    tcbReleaseRemove(sc->scTcb);

    if (period > sc->scPeriod) {
        sc->scNext += (period - sc->scPeriod);
    } else {
        sc->scNext -= (sc->scPeriod - period);
    }

    return release;
}

static inline bool_t
updateActiveBudget(sched_context_t *sc, ticks_t budget)
{
    bool_t release = false;

    /* update the active budget, this impacts running threads who have budget only */
    if (budget > sc->scBudget) {
        sc->scRemaining += (budget - sc->scBudget);
    } else {
        time_t diff = sc->scBudget - budget;
        if (diff > sc->scRemaining || sc->scRemaining - diff < getKernelWcetTicks()) {
            /* the update resulted in not enough budget, add thread to release queue at
             * the end of the update */
            release = isSchedulable(sc->scTcb);
            sc->scRemaining = 0llu;
        } else {
            sc->scRemaining -= diff;
        }
    }

    return release;
}

exception_t
invokeSchedControl_Configure(sched_context_t *target, time_t budget, time_t period, seL4_Word data)
{
    /* this boolean tells us if we need to consider
     * releasing the thread bound to the scheduling
     * context after updating all of the fields */
    bool_t release = false;

    ticks_t new_budget = usToTicks(budget);
    ticks_t new_period = usToTicks(period);

    /* if the scheduling context has a tcb, we need to consider updating
     * the currently active parameters of the scheduling context */
    if (target->scTcb) {
        /* if the previous budget was 0, we need to consider running the thread */
        if (target->scBudget == 0) {
            release = isSchedulable(target->scTcb);
        } else {
            if (target->scPeriod != new_period) {
                release = updateActivePeriod(target, new_period);
            }
            if (target->scBudget != new_budget) {
                release |= updateActiveBudget(target, new_budget);
            }
        }
    }

    /* finally alter reload parameters */
    target->scBudget = new_budget;
    target->scPeriod = new_period;

    /* we need to check if the thread is ready to be updated, otherwise postpone */
    if (release) {
        assert(isRunnable(target->scTcb));
        if (ready(target)) {
            recharge(target);
            tcbSchedEnqueue(target->scTcb);
        } else {
            tcbSchedDequeue(target->scTcb);
            postpone(target);
        }
    }

    if (target->scTcb == ksCurThread) {
        rescheduleRequired();
    }

    target->scData = data;

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
    seL4_Word data;
    seL4_Word buffer_index;
    time_t budget, period;
    cap_t targetCap;
    sched_context_t *target;

    if (extra_caps.excaprefs[0] == NULL) {
        userError("SchedControl_Configure: truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    buffer_index = 0;
    ret = arch_parseTimeArg(buffer_index, buffer);
    budget = ret.arg;
    /* todo this could just be sizeof (time_t) / sizeof(word_t) */
    buffer_index += ret.words;

    ret = arch_parseTimeArg(buffer_index, buffer);
    period = ret.arg;
    buffer_index += ret.words;

    data = getSyscallArg(buffer_index, buffer);
    buffer_index += 1;

    if (length < buffer_index) {
        userError("SchedControl_Configure: truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

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
    return invokeSchedControl_Configure(target, budget, period, data);
}

exception_t
invokeSchedControl_SetCriticality(crit_t criticality)
{
    crit_t prev = ksCriticality;

    ksCriticality = criticality;

    rescheduleRequired();

    for (crit_t i = criticality; i <= seL4_MaxCrit; i++) {
        if (i > 0) {
            tcb_t *curr = ksCritQueues[i].head;
            while (curr != NULL) {
                adjustPriorityByCriticality(curr, criticality > prev);
                curr = curr->tcbCritNext;
            }
        }
    }

    return EXCEPTION_NONE;
}

exception_t
decodeSchedControl_SetCriticality(word_t length, word_t *buffer)
{
    crit_t criticality;

    if (length < 1) {
        userError("SchedControl_Configure: truncated message");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    criticality = getSyscallArg(0, buffer);
    if (criticality > seL4_MaxCrit) {
        userError("SchedControl_Configure: criticality %lu higher than max configured criticality %u\n",
                  criticality, seL4_MaxCrit);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = seL4_MinCrit;
        current_syscall_error.rangeErrorMax = seL4_MaxCrit;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedControl_SetCriticality(criticality);
}

exception_t
decodeSchedControlInvocation(word_t label, word_t length, extra_caps_t extraCaps, word_t *buffer)
{
    switch (label) {
    case SchedControlConfigure:
        return decodeSchedControl_Configure(length, extraCaps, buffer);
    case SchedControlSetCriticality:
        return decodeSchedControl_SetCriticality(length, buffer);
    default:
        userError("SchedControl invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

