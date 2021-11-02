/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <machine/timer.h>
#include <mode/api/ipc_buffer.h>
#include <object/schedcontext.h>
#include <object/schedcontrol.h>
#include <kernel/sporadic.h>

static exception_t invokeSchedControl_ConfigureFlags(sched_context_t *target, word_t core, ticks_t budget,
                                                     ticks_t period, word_t max_refills, word_t badge, word_t flags)
{

    target->scBadge = badge;
    target->scSporadic = (flags & seL4_SchedContext_Sporadic) != 0;

    /* don't modify parameters of tcb while it is in a sorted queue */
    if (target->scTcb) {
        /* possibly stall a remote core */
        SMP_COND_STATEMENT(remoteTCBStall(target->scTcb));
        /* remove from scheduler */
        tcbReleaseRemove(target->scTcb);
        tcbSchedDequeue(target->scTcb);
        /* bill the current consumed amount before adjusting the params */
        if (NODE_STATE(ksCurSC) == target) {
            /* This could potentially mutate state but if it returns
             * true no state was modified, thus removing it should
             * be the same. */
            assert(checkBudget());
            commitTime();
        }
    }

    if (budget == period) {
        /* this is a cool hack: for round robin, we set the
         * period to 0, which means that the budget will always be ready to be refilled
         * and avoids some special casing.
         */
        REFILL_NEW(target, MIN_REFILLS, budget, 0, core);
    } else if (SMP_COND_STATEMENT(core == target->scCore &&) target->scRefillMax > 0 && target->scTcb
               && isRunnable(target->scTcb)) {
        /* the scheduling context is active - it can be used, so
         * we need to preserve the bandwidth */
        refill_update(target, period, budget, max_refills);
    } else {
        /* the scheduling context isn't active - it's budget is not being used, so
         * we can just populate the parameters from now */
        REFILL_NEW(target, max_refills, budget, period, core);
    }

#ifdef ENABLE_SMP_SUPPORT
    target->scCore = core;
    if (target->scTcb) {
        migrateTCB(target->scTcb, target->scCore);
    }
#endif /* ENABLE_SMP_SUPPORT */

    assert(target->scRefillMax > 0);
    if (target->scTcb) {
        schedContext_resume(target);
        if (SMP_TERNARY(core == CURRENT_CPU_INDEX(), true)) {
            if (isRunnable(target->scTcb) && target->scTcb != NODE_STATE(ksCurThread)) {
                possibleSwitchTo(target->scTcb);
            }
        } else if (isRunnable(target->scTcb)) {
            SCHED_ENQUEUE(target->scTcb);
        }
        if (target->scTcb == NODE_STATE(ksCurThread)) {
            rescheduleRequired();
        }
    }

    return EXCEPTION_NONE;
}

static exception_t decodeSchedControl_ConfigureFlags(word_t length, cap_t cap, word_t *buffer)
{
    if (current_extra_caps.excaprefs[0] == NULL) {
        userError("SchedControl_ConfigureFlags: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < (TIME_ARG_SIZE * 2) + 3) {
        userError("SchedControl_configureFlags: truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    time_t budget_us = mode_parseTimeArg(0, buffer);
    ticks_t budget_ticks = usToTicks(budget_us);
    time_t period_us = mode_parseTimeArg(TIME_ARG_SIZE, buffer);
    ticks_t period_ticks = usToTicks(period_us);
    word_t extra_refills = getSyscallArg(TIME_ARG_SIZE * 2, buffer);
    word_t badge = getSyscallArg(TIME_ARG_SIZE * 2 + 1, buffer);
    word_t flags = getSyscallArg(TIME_ARG_SIZE * 2 + 2, buffer);

    cap_t targetCap = current_extra_caps.excaprefs[0]->cap;
    if (unlikely(cap_get_capType(targetCap) != cap_sched_context_cap)) {
        userError("SchedControl_ConfigureFlags: target cap not a scheduling context cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (budget_us > MAX_PERIOD_US || budget_ticks < MIN_BUDGET) {
        userError("SchedControl_ConfigureFlags: budget out of range.");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = MIN_BUDGET_US;
        current_syscall_error.rangeErrorMax = MAX_PERIOD_US;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (period_us > MAX_PERIOD_US || period_ticks < MIN_BUDGET) {
        userError("SchedControl_ConfigureFlags: period out of range.");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = MIN_BUDGET_US;
        current_syscall_error.rangeErrorMax = MAX_PERIOD_US;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (budget_ticks > period_ticks) {
        userError("SchedControl_ConfigureFlags: budget must be <= period");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = MIN_BUDGET_US;
        current_syscall_error.rangeErrorMax = period_us;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (extra_refills + MIN_REFILLS > refill_absolute_max(targetCap)) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = refill_absolute_max(targetCap) - MIN_REFILLS;
        userError("Max refills invalid, got %lu, max %lu",
                  extra_refills,
                  current_syscall_error.rangeErrorMax);
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedControl_ConfigureFlags(SC_PTR(cap_sched_context_cap_get_capSCPtr(targetCap)),
                                             cap_sched_control_cap_get_core(cap),
                                             budget_ticks,
                                             period_ticks,
                                             extra_refills + MIN_REFILLS,
                                             badge,
                                             flags);
}

exception_t decodeSchedControlInvocation(word_t label, cap_t cap, word_t length, word_t *buffer)
{
    switch (label) {
    case SchedControlConfigureFlags:
        return  decodeSchedControl_ConfigureFlags(length, cap, buffer);
    default:
        userError("SchedControl invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}
