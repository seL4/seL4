/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <machine/timer.h>
#include <kernel/sporadic.h>
#include <kernel/thread.h>
#include <object/structures.h>
#include <object/schedcontext.h>

static exception_t invokeSchedContext_UnbindObject(sched_context_t *sc, cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_thread_cap:
        schedContext_unbindTCB(sc, sc->scTcb);
        break;
    case cap_notification_cap:
        schedContext_unbindNtfn(sc);
        break;
    default:
        fail("invalid cap type");
    }

    return EXCEPTION_NONE;
}

static exception_t decodeSchedContext_UnbindObject(sched_context_t *sc)
{
    if (current_extra_caps.excaprefs[0] == NULL) {
        userError("SchedContext_Unbind: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap_t cap = current_extra_caps.excaprefs[0]->cap;
    switch (cap_get_capType(cap)) {
    case cap_thread_cap:
        if (sc->scTcb != TCB_PTR(cap_thread_cap_get_capTCBPtr(cap))) {
            userError("SchedContext UnbindObject: object not bound");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if (sc->scTcb == NODE_STATE(ksCurThread)) {
            userError("SchedContext UnbindObject: cannot unbind sc of current thread");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        break;
    case cap_notification_cap:
        if (sc->scNotification != NTFN_PTR(cap_notification_cap_get_capNtfnPtr(cap))) {
            userError("SchedContext UnbindObject: object not bound");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        break;

    default:
        userError("SchedContext_Unbind: invalid cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;

    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedContext_UnbindObject(sc, cap);
}

static exception_t invokeSchedContext_Bind(sched_context_t *sc, cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_thread_cap:
        schedContext_bindTCB(sc, TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)));
        break;
    case cap_notification_cap:
        schedContext_bindNtfn(sc, NTFN_PTR(cap_notification_cap_get_capNtfnPtr(cap)));
        break;
    default:
        fail("invalid cap type");
    }

    return EXCEPTION_NONE;
}

static exception_t decodeSchedContext_Bind(sched_context_t *sc)
{
    if (current_extra_caps.excaprefs[0] == NULL) {
        userError("SchedContext_Bind: Truncated Message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap_t cap = current_extra_caps.excaprefs[0]->cap;

    switch (cap_get_capType(cap)) {
    case cap_thread_cap:
        if (sc->scTcb != NULL) {
            userError("SchedContext_Bind: sched context already bound.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (TCB_PTR(cap_thread_cap_get_capTCBPtr(cap))->tcbSchedContext != NULL) {
            userError("SchedContext_Bind: tcb already bound.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (isBlocked(TCB_PTR(cap_thread_cap_get_capTCBPtr(cap))) && !sc_released(sc)) {
            userError("SchedContext_Bind: tcb blocked and scheduling context not schedulable.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        break;
    case cap_notification_cap:
        if (sc->scNotification != NULL) {
            userError("SchedContext_Bind: sched context already bound.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (notification_ptr_get_ntfnSchedContext(NTFN_PTR(cap_notification_cap_get_capNtfnPtr(cap)))) {
            userError("SchedContext_Bind: notification already bound");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        break;
    default:
        userError("SchedContext_Bind: invalid cap.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedContext_Bind(sc, cap);
}

static exception_t invokeSchedContext_Unbind(sched_context_t *sc)
{
    schedContext_unbindAllTCBs(sc);
    schedContext_unbindNtfn(sc);
    if (sc->scReply) {
        sc->scReply->replyNext = call_stack_new(0, false);
        sc->scReply = NULL;
    }
    return EXCEPTION_NONE;
}

#ifdef ENABLE_SMP_SUPPORT
static inline void maybeStallSC(sched_context_t *sc)
{
    if (sc->scTcb) {
        remoteTCBStall(sc->scTcb);
    }
}
#endif

static inline void setConsumed(sched_context_t *sc, word_t *buffer)
{
    time_t consumed = schedContext_updateConsumed(sc);
    word_t length = mode_setTimeArg(0, consumed, buffer, NODE_STATE(ksCurThread));
    setRegister(NODE_STATE(ksCurThread), msgInfoRegister, wordFromMessageInfo(seL4_MessageInfo_new(0, 0, 0, length)));
}

static exception_t invokeSchedContext_Consumed(sched_context_t *sc, word_t *buffer)
{
    setConsumed(sc, buffer);
    return EXCEPTION_NONE;
}

static exception_t invokeSchedContext_YieldTo(sched_context_t *sc, word_t *buffer)
{
    if (sc->scYieldFrom) {
        schedContext_completeYieldTo(sc->scYieldFrom);
        assert(sc->scYieldFrom == NULL);
    }

    /* if the tcb is in the scheduler, it's ready and sufficient.
     * Otherwise, check that it is ready and sufficient and if not,
     * place the thread in the release queue. This way, from this point,
     * if the thread isSchedulable, it is ready and sufficient.*/
    schedContext_resume(sc);

    bool_t return_now = true;
    if (isSchedulable(sc->scTcb)) {
        if (SMP_COND_STATEMENT(sc->scCore != getCurrentCPUIndex() ||)
            sc->scTcb->tcbPriority < NODE_STATE(ksCurThread)->tcbPriority) {
            tcbSchedDequeue(sc->scTcb);
            SCHED_ENQUEUE(sc->scTcb);
        } else {
            NODE_STATE(ksCurThread)->tcbYieldTo = sc;
            sc->scYieldFrom = NODE_STATE(ksCurThread);
            tcbSchedDequeue(sc->scTcb);
            tcbSchedEnqueue(NODE_STATE(ksCurThread));
            tcbSchedEnqueue(sc->scTcb);
            rescheduleRequired();

            /* we are scheduling the thread associated with sc,
             * so we don't need to write to the ipc buffer
             * until the caller is scheduled again */
            return_now = false;
        }
    }

    if (return_now) {
        setConsumed(sc, buffer);
    }

    return EXCEPTION_NONE;
}

static exception_t decodeSchedContext_YieldTo(sched_context_t *sc, word_t *buffer)
{
    if (sc->scTcb == NULL) {
        userError("SchedContext_YieldTo: cannot yield to an inactive sched context");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (sc->scTcb == NODE_STATE(ksCurThread)) {
        userError("SchedContext_YieldTo: cannot seL4_SchedContext_YieldTo on self");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (sc->scTcb->tcbPriority > NODE_STATE(ksCurThread)->tcbMCP) {
        userError("SchedContext_YieldTo: insufficient mcp (%lu) to yield to a thread with prio (%lu)",
                  (unsigned long) NODE_STATE(ksCurThread)->tcbMCP, (unsigned long) sc->scTcb->tcbPriority);
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    // This should not be possible as the currently running thread
    // should never have a non-null yieldTo, however verifying this
    // invariant is being left to future work.
    assert(NODE_STATE(ksCurThread)->tcbYieldTo == NULL);
    if (NODE_STATE(ksCurThread)->tcbYieldTo != NULL) {
        userError("SchedContext_YieldTo: cannot seL4_SchedContext_YieldTo to more than on SC at a time");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedContext_YieldTo(sc, buffer);
}

exception_t decodeSchedContextInvocation(word_t label, cap_t cap, word_t *buffer)
{
    sched_context_t *sc = SC_PTR(cap_sched_context_cap_get_capSCPtr(cap));

    SMP_COND_STATEMENT((maybeStallSC(sc));)

    switch (label) {
    case SchedContextConsumed:
        /* no decode */
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return invokeSchedContext_Consumed(sc, buffer);
    case SchedContextBind:
        return decodeSchedContext_Bind(sc);
    case SchedContextUnbindObject:
        return decodeSchedContext_UnbindObject(sc);
    case SchedContextUnbind:
        /* no decode */
        if (sc->scTcb == NODE_STATE(ksCurThread)) {
            userError("SchedContext UnbindObject: cannot unbind sc of current thread");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return invokeSchedContext_Unbind(sc);
    case SchedContextYieldTo:
        return decodeSchedContext_YieldTo(sc, buffer);
    default:
        userError("SchedContext invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

void schedContext_resume(sched_context_t *sc)
{
    assert(!sc || sc->scTcb != NULL);
    if (likely(sc) && isSchedulable(sc->scTcb)) {
        if (!(refill_ready(sc) && refill_sufficient(sc, 0))) {
            assert(!thread_state_get_tcbQueued(sc->scTcb->tcbState));
            postpone(sc);
        }
    }
}

void schedContext_bindTCB(sched_context_t *sc, tcb_t *tcb)
{
    assert(sc->scTcb == NULL);
    assert(tcb->tcbSchedContext == NULL);

    tcb->tcbSchedContext = sc;
    sc->scTcb = tcb;

    SMP_COND_STATEMENT(migrateTCB(tcb, sc->scCore));

    if (sc_sporadic(sc) && sc_active(sc) && sc != NODE_STATE(ksCurSC)) {
        refill_unblock_check(sc);
    }
    schedContext_resume(sc);
    if (isSchedulable(tcb)) {
        SCHED_ENQUEUE(tcb);
        rescheduleRequired();
        // TODO -- at some stage we should take this call out of any TCB invocations that
        // alter capabilities, so that we can do a direct switch. The prefernce here is to
        // remove seL4_SetSchedParams from using ThreadControl. It's currently out of scope for
        // verification work, so the work around is to use rescheduleRequired()
        //possibleSwitchTo(tcb);
    }
}

void schedContext_unbindTCB(sched_context_t *sc, tcb_t *tcb)
{
    assert(sc->scTcb == tcb);

    /* tcb must already be stalled at this point */
    if (tcb == NODE_STATE(ksCurThread)) {
        rescheduleRequired();
    }

    tcbSchedDequeue(sc->scTcb);
    tcbReleaseRemove(sc->scTcb);

    sc->scTcb->tcbSchedContext = NULL;
    sc->scTcb = NULL;
}

void schedContext_unbindAllTCBs(sched_context_t *sc)
{
    if (sc->scTcb) {
        SMP_COND_STATEMENT(remoteTCBStall(sc->scTcb));
        schedContext_unbindTCB(sc, sc->scTcb);
    }
}

void schedContext_donate(sched_context_t *sc, tcb_t *to)
{
    assert(sc != NULL);
    assert(to != NULL);
    assert(to->tcbSchedContext == NULL);

    tcb_t *from = sc->scTcb;
    if (from) {
        SMP_COND_STATEMENT(remoteTCBStall(from));
        tcbSchedDequeue(from);
        tcbReleaseRemove(from);
        from->tcbSchedContext = NULL;
        if (from == NODE_STATE(ksCurThread) || from == NODE_STATE(ksSchedulerAction)) {
            rescheduleRequired();
        }
    }
    sc->scTcb = to;
    to->tcbSchedContext = sc;

    SMP_COND_STATEMENT(migrateTCB(to, sc->scCore));
}

void schedContext_bindNtfn(sched_context_t *sc, notification_t *ntfn)
{
    notification_ptr_set_ntfnSchedContext(ntfn, SC_REF(sc));
    sc->scNotification = ntfn;
}

void schedContext_unbindNtfn(sched_context_t *sc)
{
    if (sc && sc->scNotification) {
        notification_ptr_set_ntfnSchedContext(sc->scNotification, SC_REF(0));
        sc->scNotification = NULL;
    }
}

time_t schedContext_updateConsumed(sched_context_t *sc)
{
    ticks_t consumed = sc->scConsumed;
    if (consumed >= getMaxTicksToUs()) {
        sc->scConsumed -= getMaxTicksToUs();
        return ticksToUs(getMaxTicksToUs());
    } else {
        sc->scConsumed = 0;
        return ticksToUs(consumed);
    }
}

void schedContext_cancelYieldTo(tcb_t *tcb)
{
    if (tcb && tcb->tcbYieldTo) {
        tcb->tcbYieldTo->scYieldFrom = NULL;
        tcb->tcbYieldTo = NULL;
    }
}

void schedContext_completeYieldTo(tcb_t *yielder)
{
    if (yielder && yielder->tcbYieldTo) {
        setConsumed(yielder->tcbYieldTo, lookupIPCBuffer(true, yielder));
        schedContext_cancelYieldTo(yielder);
    }
}
