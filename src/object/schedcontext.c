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
decodeSchedContextInvocation(word_t label, cap_t cap)
{
    switch (label) {
    case SchedContextYield:
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeSchedContext_Yield(SC_PTR(cap_sched_context_cap_get_capPtr(cap)));
    default:
        userError("SchedContext invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

