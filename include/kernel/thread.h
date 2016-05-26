/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_THREAD_H
#define __KERNEL_THREAD_H

#include <types.h>
#include <util.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <machine/timer.h>

static inline CONST word_t
prio_to_l1index(word_t prio)
{
    return (prio >> wordRadix);
}

static inline CONST word_t
l1index_to_prio(word_t l1index)
{
    return (l1index << wordRadix);
}

static inline CONST word_t
invert_l1index(word_t l1index)
{
    word_t inverted = (L2_BITMAP_SIZE - 1 - l1index);
    assert(inverted < L2_BITMAP_SIZE);
    return inverted;
}

static inline PURE word_t
highestPrio(void)
{
    word_t l1index;
    word_t l2index;
    word_t l1index_inverted;

    /* it's undefined to call clzl on 0 */
    assert(ksReadyQueuesL1Bitmap != 0);

    l1index = wordBits - 1 - clzl(ksReadyQueuesL1Bitmap);
    l1index_inverted = invert_l1index(l1index);
    assert(ksReadyQueuesL2Bitmap[l1index_inverted] != 0);
    l2index = wordBits - 1 - clzl(ksReadyQueuesL2Bitmap[l1index_inverted]);

    return (l1index_to_prio(l1index) | l2index);
}

/* return true if the threads scheduling context's
 * period has expired and the budget is ready to be
 * recharged
 */
static inline bool_t
ready(sched_context_t *sc)
{
    return (ksCurrentTime + getKernelWcetTicks()) >= sc->scNext;
}

static inline bool_t
currentThreadExpired(void)
{
    return ksCurThread->tcbSchedContext->scRemaining < (ksConsumed + getKernelWcetTicks());
}

static inline bool_t PURE
isRunnable(const tcb_t *thread)
{
    return thread_state_get_tsType(thread->tcbState) >= ThreadState_Running;
}

static inline bool_t PURE
isSchedulable(const tcb_t *thread)
{
    return isRunnable(thread) && thread->tcbSchedContext != NULL &&
           !thread_state_get_tcbInReleaseQueue(thread->tcbState);
}

void configureIdleThread(tcb_t *tcb);
void activateThread(void) VISIBLE;
void suspend(tcb_t *target);
void restart(tcb_t *target);
void doIPCTransfer(tcb_t *sender, endpoint_t *endpoint,
                   word_t badge, bool_t grant, tcb_t *receiver);
void doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot);
void doNormalTransfer(tcb_t *sender, word_t *sendBuffer, endpoint_t *endpoint,
                      word_t badge, bool_t canGrant, tcb_t *receiver,
                      word_t *receiveBuffer);
void doFaultTransfer(word_t badge, tcb_t *sender, tcb_t *receiver,
                     word_t *receiverIPCBuffer);
void doNBRecvFailedTransfer(tcb_t *thread);
void schedule(void);
void chooseThread(void);
void switchToThread(tcb_t *thread) VISIBLE;
void switchToIdleThread(void);
void switchSchedContext(void) VISIBLE;
void setPriorityFields(tcb_t *tptr, seL4_Prio_t prio);
void setActivePriority(tcb_t *tptr, prio_t prio);
void scheduleTCB(tcb_t *tptr);
void attemptSwitchTo(tcb_t *tptr);
void switchIfRequiredTo(tcb_t *tptr);
void setThreadState(tcb_t *tptr, _thread_state_t ts) VISIBLE;
void updateTimestamp(void);
bool_t checkBudget(void);
void endTimeslice(sched_context_t *sc);
void rescheduleRequired(void);
void recharge(sched_context_t *sc);
void awaken(void);
void postpone(sched_context_t *sc);
void adjustPriorityByCriticality(tcb_t *tcb, bool_t up);
#endif
