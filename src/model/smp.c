/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <model/smp.h>
#include <object/tcb.h>

#ifdef ENABLE_SMP_SUPPORT

void migrateTCB(tcb_t *tcb, word_t new_core)
{
    if (new_core == tcb->tcbAffinity) {
        return;
    }
#ifdef CONFIG_DEBUG_BUILD
    tcbDebugRemove(tcb);
#endif
#ifdef CONFIG_HAVE_FPU
    /* If the thread owns the FPU of the core it is currently running on (which
     * is not necessarily the core, that we are now running on), then release
     * that cores's FPU.
     */
    fpuRelease(tcb);
#endif /* CONFIG_HAVE_FPU */
    if (tcb == NODE_STATE(ksCurThread)) {
        /**
         * Switch the current thread to the idle thread. This is necessary to
         * preserve the invariant the the current thread (NODE_STATE(ksCurThread))
         * variable is always running on the current core.
         *
         */
        switchToIdleThread();

        /* Like rescheduleRequired but specific.
         * This is broadly similar to how invokeTCB_SetAffinity() behaves.
         */
        tcb_t *action = NODE_STATE(ksSchedulerAction);
        if (action == tcb || action == SchedulerAction_ResumeCurrentThread) {
            NODE_STATE(ksSchedulerAction) = SchedulerAction_ChooseNewThread;
        }

        tcb->tcbAffinity = new_core;
        if (isSchedulable(tcb)) {
            SCHED_ENQUEUE(tcb);
        }
    } else {
        tcb->tcbAffinity = new_core;
    }
#ifdef CONFIG_DEBUG_BUILD
    tcbDebugAppend(tcb);
#endif
}

#endif /* ENABLE_SMP_SUPPORT */
