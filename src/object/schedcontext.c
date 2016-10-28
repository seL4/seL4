/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
#include <machine/timer.h>
#include <object/schedcontext.h>

static exception_t invokeSchedContext_UnbindObject(sched_context_t *sc, cap_t cap)
{
    schedContext_unbindTCB(sc, TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)));
    return EXCEPTION_NONE;
}

static exception_t decodeSchedContext_UnbindObject(sched_context_t *sc, extra_caps_t extraCaps)
{
    if (extraCaps.excaprefs[0] == NULL) {
        userError("SchedContext_Unbind: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap_t cap = extraCaps.excaprefs[0]->cap;
    if (cap_get_capType(cap) != cap_thread_cap) {
        userError("SchedContext_Unbind: invalid cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (sc->scTcb != TCB_PTR(cap_thread_cap_get_capTCBPtr(cap))) {
        userError("SchedContext_Unbind: object not bound to this sc");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedContext_UnbindObject(sc, cap);
}

static exception_t invokeSchedContext_Bind(sched_context_t *sc, cap_t cap)
{
    schedContext_bindTCB(sc, TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)));
    return EXCEPTION_NONE;
}

static exception_t decodeSchedContext_Bind(sched_context_t *sc, extra_caps_t extraCaps)
{
    if (extraCaps.excaprefs[0] == NULL) {
        userError("SchedContext_Bind: Truncated Message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap_t cap = extraCaps.excaprefs[0]->cap;

    if (sc->scTcb != NULL) {
        userError("SchedContext_Bind: sched context already bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_get_capType(cap) != cap_thread_cap) {
        userError("SchedContext_Bind: invalid cap.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (TCB_PTR(cap_thread_cap_get_capTCBPtr(cap))->tcbSchedContext != NULL) {
        userError("SchedContext_Bind: tcb already bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeSchedContext_Bind(sc, cap);
}

static exception_t invokeSchedContext_Unbind(sched_context_t *sc)
{
    schedContext_unbindAllTCBs(sc);
    return EXCEPTION_NONE;
}

exception_t decodeSchedContextInvocation(word_t label, cap_t cap, extra_caps_t extraCaps)
{
    sched_context_t *sc = SC_PTR(cap_sched_context_cap_get_capSCPtr(cap));

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

void schedContext_resume(sched_context_t *sc)
{
    assert(sc->scTcb != NULL);
#if CONFIG_MAX_NUM_NODES > 1
    /* this should NOT be called when migration is possible */
    assert(sc->scCore == getCurrentCPUIndex());
#endif
    if (isRunnable(sc->scTcb) && sc->scBudget > 0) {
        recharge(sc);
        possibleSwitchTo(sc->scTcb);
    }
}

void schedContext_bindTCB(sched_context_t *sc, tcb_t *tcb)
{
    assert(sc->scTcb == NULL);
    assert(tcb->tcbSchedContext == NULL);

    tcb->tcbSchedContext = sc;
    sc->scTcb = tcb;
#if CONFIG_MAX_NUM_NODES > 1
    if (thread->tcbAffinity != sc->scCore) {
        if (nativeThreadUsingFPU(tcb)) {
            switchFPUOwner(NULL, thread->tcbAffinity);
        }
        tcbSchedDequeue(tcb);
        tcb->tcbAffinity = sc->scCore;
    }
#endif
    schedContext_resume(sc);
}

void schedContext_unbindTCB(sched_context_t *sc, tcb_t *tcb)
{
    assert(sc->scTcb == tcb);

    tcbSchedDequeue(sc->scTcb);

    sc->scTcb->tcbSchedContext = NULL;
    if (sc->scTcb == NODE_STATE(ksCurThread)) {
        rescheduleRequired();
    } else {
        SMP_COND_STATEMENT(remoteTCBStall(tcb));
    }

    sc->scTcb = NULL;
}

void schedContext_unbindAllTCBs(sched_context_t *sc)
{
    if (sc->scTcb) {
        schedContext_unbindTCB(sc, sc->scTcb);
    }
}
