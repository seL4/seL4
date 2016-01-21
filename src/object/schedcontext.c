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
#include <object/structures.h>
#include <kernel/thread.h>
#include <model/statedata.h>
#include <object/tcb.h>

#include <object/schedcontext.h>

exception_t
invokeSchedContext_Yield(sched_context_t *sc)
{
    if (likely(sc->scTcb && isSchedulable(sc->scTcb))) {
        endTimeslice(sc);
        if (likely(sc->scTcb == ksCurThread)) {
            ksConsumed = 0llu;
            rescheduleRequired();
        }
    } else {
        recharge(sc);
    }

    return EXCEPTION_NONE;
}

exception_t
invokeSchedContext_UnbindTCB(sched_context_t *sc)
{
    schedContext_unbindTCB(sc);
    return EXCEPTION_NONE;
}

exception_t
invokeSchedContext_BindTCB(sched_context_t *sc, tcb_t *tcb)
{
    schedContext_bindTCB(sc, tcb);
    return EXCEPTION_NONE;
}

exception_t
decodeSchedContext_BindTCB(sched_context_t *sc, extra_caps_t rootCaps)
{
    cap_t cap;
    tcb_t *tcb;

    if (rootCaps.excaprefs[0] == NULL) {
        userError("SchedContext BindTCB: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap = rootCaps.excaprefs[0]->cap;

    if (cap_get_capType(cap) != cap_thread_cap) {
        userError("SchedContext BindTCB: TCB cap invalid.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (sc->scTcb != NULL) {
        userError("SchedContext_BindTCB: scheduling context already bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedContext_BindTCB(sc, tcb);
}

exception_t
invokeSchedContext_UnbindNtfn(sched_context_t *sc)
{
    schedContext_unbindNtfn(sc);
    return EXCEPTION_NONE;
}

exception_t
invokeSchedContext_BindNtfn(sched_context_t *sc, notification_t *ntfn)
{
    schedContext_bindNtfn(sc, ntfn);
    return EXCEPTION_NONE;
}

exception_t
decodeSchedContext_BindNtfn(sched_context_t *sc, extra_caps_t rootCaps)
{
    cap_t cap;
    notification_t *ntfn;

    if (unlikely(rootCaps.excaprefs[0] == NULL)) {
        userError("SchedContext BindNotification: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap = rootCaps.excaprefs[0]->cap;

    if (unlikely(cap_get_capType(cap) != cap_notification_cap)) {
        userError("SchedContext BindNotification: Notification cap invalid.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(sc->scNotification != NULL)) {
        userError("SchedContext_BindNotification: scheduling context already bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    ntfn = NTFN_PTR(cap_notification_cap_get_capNtfnPtr(cap));
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedContext_BindNtfn(sc, ntfn);
}


exception_t
decodeSchedContextInvocation(word_t label, cap_t cap, extra_caps_t extraCaps)
{

    sched_context_t *sc = SC_PTR(cap_sched_context_cap_get_capPtr(cap));

    switch (label) {
    case SchedContextYield:
        /* no decode stage */
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeSchedContext_Yield(sc);
    case SchedContextBindTCB:
        return decodeSchedContext_BindTCB(sc, extraCaps);
    case SchedContextUnbindTCB:
        /* no decode stage */
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeSchedContext_UnbindTCB(sc);
    case SchedContextBindNotification:
        return decodeSchedContext_BindNtfn(sc, extraCaps);
    case SchedContextUnbindNotification:
        /* no decode stage */
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeSchedContext_UnbindNtfn(sc);
    default:
        userError("SchedContext invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

void
schedContext_resume(sched_context_t *sc)
{
    if (isRunnable(sc->scTcb) && sc->scBudget > 0) {
        if (ready(sc)) {
            recharge(sc);
        }

        if (sc->scRemaining < getKernelWcetTicks()) {
            postpone(sc);
        } else {
            switchIfRequiredTo(sc->scTcb);
        }
    }
}

void
schedContext_bindTCB(sched_context_t *sc, tcb_t *tcb)
{
    tcb->tcbSchedContext = sc;
    sc->scTcb = tcb;
    schedContext_resume(sc);
}

void
schedContext_unbindTCB(sched_context_t *sc)
{
    /* pause the tcb without effecting it's state -
     * it will get to run again when it receives a scheduling
     * context.
     */
    if (sc && sc->scTcb) {
        tcbSchedDequeue(sc->scTcb);
        tcbReleaseRemove(sc->scTcb);

        if (sc->scTcb == ksCurThread) {
            rescheduleRequired();
        }

        sc->scTcb->tcbSchedContext = NULL;
        sc->scTcb = NULL;
    }
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

void
schedContext_donate(tcb_t *to, sched_context_t *sc)
{
    assert(to != NULL);
    assert(sc != NULL);

    if (sc->scTcb) {
        /* thread must not be in the scheduling queue */
        assert(!thread_state_get_tcbQueued(sc->scTcb->tcbState));
        sc->scTcb->tcbSchedContext = NULL;
    }
    sc->scTcb = to;
    to->tcbSchedContext = sc;
}

