/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <api/failures.h>
#include <model/preemption.h>
#include <model/statedata.h>
#include <plat/machine/hardware.h>
#include <config.h>

/*
 * Possibly preempt the current thread to allow an interrupt to be handled.
 */
exception_t preemptionPoint(void)
{
    /* Record that we have performed some work. */
    ksWorkUnitsCompleted++;

    /*
     * If we have performed a non-trivial amount of work since last time we
     * checked for preemption, and there is an interrupt pending, handle the
     * interrupt.
     *
     * We avoid checking for pending IRQs every call, as our callers tend to
     * call us in a tight loop and checking for pending IRQs can be quite slow.
     */
    if (ksWorkUnitsCompleted >= CONFIG_MAX_NUM_WORK_UNITS_PER_PREEMPTION) {
        ksWorkUnitsCompleted = 0;
#ifdef CONFIG_KERNEL_MCS
        updateTimestamp();
        if (!(sc_active(NODE_STATE(ksCurSC)) && refill_sufficient(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed)))
            || isCurDomainExpired() || isIRQPending()) {
#else
        if (isIRQPending()) {
#endif
            return EXCEPTION_PREEMPTED;
        }
    }

    return EXCEPTION_NONE;
}

