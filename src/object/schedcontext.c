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
#include <kernel/thread.h>
#include <model/statedata.h>
#include <object/tcb.h>

#include <object/schedcontext.h>

exception_t
invokeSchedContext_Yield(sched_context_t *sc)
{
    if (likely(sc->tcb && isSchedulable(sc->tcb))) {
        endTimeslice(sc);
        if (likely(sc->tcb == ksCurThread)) {
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
    unbindSchedContext(sc);
    return EXCEPTION_NONE;
}

exception_t
invokeSchedContext_BindTCB(sched_context_t *sc, tcb_t *tcb)
{
    bindSchedContext(sc, tcb);
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

    if (sc->tcb != NULL) {
        userError("SchedContext_BindTCB: scheduling context already bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedContext_BindTCB(sc, tcb);
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
    default:
        userError("SchedContext invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

void
bindSchedContext(sched_context_t *sc, tcb_t *tcb)
{
    tcb->tcbSchedContext = sc;
    sc->tcb = tcb;

    if (isRunnable(tcb)) {
        if (ready(sc)) {
            recharge(sc);
        }

        if (sc->remaining < getKernelWcetTicks()) {
            postpone(sc);
        } else {
            switchIfRequiredTo(tcb, false);
        }
    }
}

void
unbindSchedContext(sched_context_t *sc)
{
    /* pause the tcb without effecting it's state -
     * it will get to run again when it receives a scheduling
     * context.
     */
    if (sc && sc->tcb) {
        tcbSchedDequeue(sc->tcb);
        tcbReleaseRemove(sc->tcb);

        if (sc->tcb == ksCurThread) {
            rescheduleRequired();
        }

        sc->tcb->tcbSchedContext = NULL;
        sc->tcb = NULL;
    }
}

