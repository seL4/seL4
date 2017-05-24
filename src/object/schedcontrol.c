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
#include <kernel/sporadic.h>

static exception_t
invokeSchedControl_Configure(sched_context_t *target, word_t core, ticks_t budget,
                             ticks_t period, word_t max_refills)
{

    /* don't modify parameters of tcb while it is in a sorted queue */
    if (target->scTcb) {
        SMP_COND_STATEMENT(remoteTCBStall(target->scTcb));
        tcbReleaseRemove(target->scTcb);
        tcbSchedDequeue(target->scTcb);
    }

    if (budget == period) {
        /* this is a cool hack: for round robin, we set the
         * period to 0, which means that the budget will always be ready to be refilled
         * and avoids some special casing.
         */
        period = 0;
        max_refills = MIN_REFILLS;
    }

    if (core == target->scCore && target->scRefillMax > 0 && target->scTcb && isRunnable(target->scTcb)) {
        /* the scheduling context is active - it can be used, so
         * we need to preserve the bandwidth */
        /* remove from scheduler */
        tcbSchedDequeue(target->scTcb);
        tcbReleaseRemove(target->scTcb);

        if (NODE_STATE(ksCurSC) == target) {
            /* bill the current consumed amount before adjusting the params */
            ticks_t capacity = refill_capacity(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed));
            chargeBudget(capacity, NODE_STATE(ksConsumed));
        }
        refill_update(target, period, budget, max_refills);
    } else {
        /* the scheduling context isn't active - it's budget is not being used, so
         * we can just populate the parameters from now */
        refill_new(target, max_refills, budget, period);

        if (core != target->scCore && target->scTcb) {
            /* if the core changed and the SC has a tcb, the SC is getting
             * budget - so migrate it */
            target->scCore = core;
            SMP_COND_STATEMENT(Arch_migrateTCB(target->scTcb));
            SMP_COND_STATEMENT(target->scTcb->tcbAffinity = target->scCore;)
        }
    }

    if (target->scTcb && target->scRefillMax > 0) {
        schedContext_resume(target);
        if (target->scTcb == NODE_STATE(ksCurThread)) {
            rescheduleRequired();
        } else if (isRunnable(target->scTcb)) {
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

    if (length < (TIME_ARG_SIZE * 2) + 1) {
        userError("SchedControl_configure: truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    time_t budget_us = mode_parseTimeArg(0, buffer);
    time_t period_us = mode_parseTimeArg(TIME_ARG_SIZE, buffer);
    word_t extra_refills = getSyscallArg(TIME_ARG_SIZE * 2, buffer);

    cap_t targetCap = extraCaps.excaprefs[0]->cap;
    if (unlikely(cap_get_capType(targetCap) != cap_sched_context_cap)) {
        userError("SchedControl_Configure: target cap not a scheduling context cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (budget_us > getMaxTimerUs() || budget_us < MIN_BUDGET_US) {
        userError("SchedControl_Configure: budget out of range.");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = MIN_BUDGET_US;
        current_syscall_error.rangeErrorMax = getMaxTimerUs();
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (period_us > getMaxTimerUs() || period_us < MIN_BUDGET_US) {
        userError("SchedControl_Configure: period out of range.");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = MIN_BUDGET_US;
        current_syscall_error.rangeErrorMax = getMaxTimerUs();
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (budget_us > period_us) {
        userError("SchedControl_Configure: budget must be <= period");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = MIN_BUDGET_US;
        current_syscall_error.rangeErrorMax = period_us;
        return EXCEPTION_SYSCALL_ERROR;
    }

    sched_context_t *sc = SC_PTR(cap_sched_context_cap_get_capSCPtr(targetCap));
    if (extra_refills + MIN_REFILLS > refill_absolute_max(sc)) {
        userError("Max refills invalid");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = refill_absolute_max(sc) - MIN_REFILLS;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedControl_Configure(sc,
                                        cap_sched_control_cap_get_core(cap),
                                        usToTicks(budget_us),
                                        usToTicks(period_us),
                                        extra_refills + MIN_REFILLS);
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
