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

#include <object/schedcontext.h>

static exception_t
invokeSchedContext_UnbindObject(sched_context_t *sc, cap_t cap)
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

static exception_t
decodeSchedContext_UnbindObject(sched_context_t *sc, extra_caps_t extraCaps)
{
    if (extraCaps.excaprefs[0] == NULL) {
        userError("SchedContext_Unbind: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap_t cap = extraCaps.excaprefs[0]->cap;
    switch (cap_get_capType(cap)) {
    case cap_thread_cap:
        if (sc->scTcb != TCB_PTR(cap_thread_cap_get_capTCBPtr(cap))) {
            userError("SchedContext UnbindObject: object not bound");
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

static exception_t
invokeSchedContext_Bind(sched_context_t *sc, cap_t cap)
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

static exception_t
decodeSchedContext_Bind(sched_context_t *sc, extra_caps_t extraCaps)
{
    if (extraCaps.excaprefs[0] == NULL) {
        userError("SchedContext_Bind: Truncated Message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap_t cap = extraCaps.excaprefs[0]->cap;

    if (sc->scTcb != NULL || sc->scNotification != NULL) {
        userError("SchedContext_Bind: sched context already bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    switch (cap_get_capType(cap)) {
    case cap_thread_cap:
        if (TCB_PTR(cap_thread_cap_get_capTCBPtr(cap))->tcbSchedContext != NULL) {
            userError("SchedContext_Bind: tcb already bound.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        break;
    case cap_notification_cap:
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

static exception_t
invokeSchedContext_Unbind(sched_context_t *sc)
{
    schedContext_unbindAllTCBs(sc);
    schedContext_unbindNtfn(sc);
    return EXCEPTION_NONE;
}

#if CONFIG_MAX_NUM_NODES > 1
static inline void
maybeStallSC(sched_context_t *sc)
{
    if (sc->scTcb) {
        remoteTCBStall(sc->scTcb);
    }
}
#endif

exception_t
decodeSchedContextInvocation(word_t label, cap_t cap, extra_caps_t extraCaps)
{
    sched_context_t *sc = SC_PTR(cap_sched_context_cap_get_capSCPtr(cap));

    SMP_COND_STATEMENT((maybeStallSC(sc));)

    switch (label) {
    case SchedContextBind:
        return decodeSchedContext_Bind(sc, extraCaps);
    case SchedContextUnbindObject:
        return decodeSchedContext_UnbindObject(sc, extraCaps);
    case SchedContextUnbind:
        /* no decode */
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return invokeSchedContext_Unbind(sc);
    default:
        userError("SchedContext invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

void
schedContext_resume(sched_context_t *sc)
{
    assert(!sc || sc->scTcb != NULL);
    if (likely(sc) && isSchedulable(sc->scTcb)) {
        assert(sc->scTcb != NULL);

        if (isRunnable(sc->scTcb) && sc->scRefillMax > 0) {
            if (!(refill_ready(sc) && refill_sufficient(sc, 0))) {
                assert(!thread_state_get_tcbQueued(sc->scTcb->tcbState));
                postpone(sc);
            }
        }
    }
}

void
schedContext_bindTCB(sched_context_t *sc, tcb_t *tcb)
{
    assert(sc->scTcb == NULL);
    assert(tcb->tcbSchedContext == NULL);

    tcb->tcbSchedContext = sc;
    sc->scTcb = tcb;

#if CONFIG_MAX_NUM_NODES > 1
    if (tcb->tcbAffinity != sc->scCore) {
        if (isSchedulable(tcb)) {
            SMP_COND_STATEMENT(remoteTCBStall(tcb));
            tcbSchedDequeue(tcb);
        }
        Arch_migrateTCB(tcb);
        tcb->tcbAffinity = tcb->tcbSchedContext->scCore;
    }
#endif

    schedContext_resume(sc);
    if (isSchedulable(tcb)) {
        switchIfRequiredTo(tcb);
    }
}

void schedContext_unbindTCB(sched_context_t *sc, tcb_t *tcb)
{
    assert(sc->scTcb == tcb);

    SMP_COND_STATEMENT(remoteTCBStall(tcb));
    if (tcb == NODE_STATE(ksCurThread)) {
        rescheduleRequired();
    }

    tcbSchedDequeue(sc->scTcb);
    tcbReleaseRemove(sc->scTcb);

    sc->scTcb->tcbSchedContext = NULL;
    sc->scTcb = NULL;
}

void
schedContext_unbindAllTCBs(sched_context_t *sc)
{
    if (sc->scTcb) {
        SMP_COND_STATEMENT(remoteTCBStall(sc->scTcb));
        schedContext_unbindTCB(sc, sc->scTcb);
    }
}

void
schedContext_donate(sched_context_t *sc, tcb_t *to)
{
    assert(sc != NULL);
    assert(to != NULL);
    assert(to->tcbSchedContext == NULL);

    tcb_t *from = sc->scTcb;
    if (from) {
        if (from == NODE_STATE(ksCurThread)) {
            rescheduleRequired();
        } else if (isRunnable(from)) {
            SMP_COND_STATEMENT(remoteTCBStall(from));
            tcbSchedDequeue(from);
        }
        from->tcbSchedContext = NULL;
    }
    sc->scTcb = to;
    to->tcbSchedContext = sc;

    SMP_COND_STATEMENT(remoteTCBStall(to));
    SMP_COND_STATEMENT(Arch_migrateTCB(to));
    SMP_COND_STATEMENT(to->tcbAffinity = to->tcbSchedContext->scCore;)
}

void
schedContext_bindNtfn(sched_context_t *sc, notification_t *ntfn)
{
    notification_ptr_set_ntfnSchedContext(ntfn, SC_REF(sc));
    sc->scNotification = ntfn;
}

void
schedContext_unbindNtfn(sched_context_t *sc)
{
    if (sc && sc->scNotification) {
        notification_ptr_set_ntfnSchedContext(sc->scNotification, SC_REF(0));
        sc->scNotification = NULL;
    }
}
