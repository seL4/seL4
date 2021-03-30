/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <object/structures.h>
#include <object/schedcontext.h>

void sendSignal(notification_t *ntfnPtr, word_t badge);
void receiveSignal(tcb_t *thread, cap_t cap, bool_t isBlocking);
void cancelAllSignals(notification_t *ntfnPtr);
void cancelSignal(tcb_t *threadPtr, notification_t *ntfnPtr);
void completeSignal(notification_t *ntfnPtr, tcb_t *tcb);
void unbindMaybeNotification(notification_t *ntfnPtr);
void unbindNotification(tcb_t *tcb);
void bindNotification(tcb_t *tcb, notification_t *ntfnPtr);
#ifdef CONFIG_KERNEL_MCS
void reorderNTFN(notification_t *notification, tcb_t *thread);

static inline void maybeReturnSchedContext(notification_t *ntfnPtr, tcb_t *tcb)
{

    sched_context_t *sc = SC_PTR(notification_ptr_get_ntfnSchedContext(ntfnPtr));
    if (sc == tcb->tcbSchedContext) {
        tcb->tcbSchedContext = NULL;
        sc->scTcb = NULL;
        /* If the current thread returns its sched context then it should not
           by default continue running. */
        if (tcb == NODE_STATE(ksCurThread)) {
            rescheduleRequired();
        }
    }
}
#endif

#ifdef CONFIG_KERNEL_MCS
static inline void maybeDonateSchedContext(tcb_t *tcb, notification_t *ntfnPtr)
{
    if (tcb->tcbSchedContext == NULL) {
        sched_context_t *sc = SC_PTR(notification_ptr_get_ntfnSchedContext(ntfnPtr));
        if (sc != NULL && sc->scTcb == NULL) {
            schedContext_donate(sc, tcb);
            if (sc != NODE_STATE(ksCurSC)) {
                /* refill_unblock_check should not be called on the
                 * current SC as it is already running. The current SC
                 * may have been bound to a notificaiton object if the
                 * current thread was deleted in a long-running deletion
                 * that became preempted. */
                refill_unblock_check(sc);
            }
            schedContext_resume(sc);
        }
    }
}

static inline void ntfn_set_active(notification_t *ntfnPtr, word_t badge)
{
    notification_ptr_set_state(ntfnPtr, NtfnState_Active);
    notification_ptr_set_ntfnMsgIdentifier(ntfnPtr, badge);
}


#endif
