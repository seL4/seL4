/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <util.h>
#include <object/structures.h>
#include <arch/machine.h>
#ifdef CONFIG_KERNEL_MCS
#include <kernel/sporadic.h>
#include <machine/timer.h>
#include <mode/machine.h>
#endif

static inline CONST word_t ready_queues_index(word_t dom, word_t prio)
{
    if (CONFIG_NUM_DOMAINS > 1) {
        return dom * CONFIG_NUM_PRIORITIES + prio;
    } else {
        assert(dom == 0);
        return prio;
    }
}

static inline CONST word_t prio_to_l1index(word_t prio)
{
    return (prio >> wordRadix);
}

static inline CONST word_t l1index_to_prio(word_t l1index)
{
    return (l1index << wordRadix);
}

static inline bool_t PURE isRunnable(const tcb_t *thread)
{
    switch (thread_state_get_tsType(thread->tcbState)) {
    case ThreadState_Running:
    case ThreadState_Restart:
#ifdef CONFIG_VTX
    case ThreadState_RunningVM:
#endif
        return true;

    default:
        return false;
    }
}

static inline CONST word_t invert_l1index(word_t l1index)
{
    word_t inverted = (L2_BITMAP_SIZE - 1 - l1index);
    assert(inverted < L2_BITMAP_SIZE);
    return inverted;
}

static inline prio_t getHighestPrio(word_t dom)
{
    word_t l1index;
    word_t l2index;
    word_t l1index_inverted;

    /* it's undefined to call clzl on 0 */
    assert(NODE_STATE(ksReadyQueuesL1Bitmap)[dom] != 0);

    l1index = wordBits - 1 - clzl(NODE_STATE(ksReadyQueuesL1Bitmap)[dom]);
    l1index_inverted = invert_l1index(l1index);
    assert(NODE_STATE(ksReadyQueuesL2Bitmap)[dom][l1index_inverted] != 0);
    l2index = wordBits - 1 - clzl(NODE_STATE(ksReadyQueuesL2Bitmap)[dom][l1index_inverted]);
    return (l1index_to_prio(l1index) | l2index);
}

static inline bool_t isHighestPrio(word_t dom, prio_t prio)
{
    return NODE_STATE(ksReadyQueuesL1Bitmap)[dom] == 0 ||
           prio >= getHighestPrio(dom);
}

static inline bool_t PURE isBlocked(const tcb_t *thread)
{
    switch (thread_state_get_tsType(thread->tcbState)) {
    case ThreadState_BlockedOnReceive:
    case ThreadState_BlockedOnSend:
    case ThreadState_BlockedOnNotification:
    case ThreadState_BlockedOnReply:
        return true;

    default:
        return false;
    }
}

static inline bool_t PURE isStopped(const tcb_t *thread)
{
    switch (thread_state_get_tsType(thread->tcbState)) {
    case ThreadState_Inactive:
    case ThreadState_BlockedOnReceive:
    case ThreadState_BlockedOnSend:
    case ThreadState_BlockedOnNotification:
    case ThreadState_BlockedOnReply:
        return true;

    default:
        return false;
    }
}

#ifdef CONFIG_KERNEL_MCS
static inline bool_t PURE isRoundRobin(sched_context_t *sc)
{
    return sc->scPeriod == 0;
}

static inline bool_t isCurDomainExpired(void)
{
    return CONFIG_NUM_DOMAINS > 1 &&
           ksDomainTime < (NODE_STATE(ksConsumed) + MIN_BUDGET);
}

static inline void commitTime(void)
{
    if (NODE_STATE(ksCurSC)->scRefillMax) {
        if (likely(NODE_STATE(ksConsumed) > 0)) {
            /* if this function is called the head refil must be sufficient to
             * charge ksConsumed */
            assert(refill_sufficient(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed)));
            /* and it must be ready to use */
            assert(refill_ready(NODE_STATE(ksCurSC)));

            if (isRoundRobin(NODE_STATE(ksCurSC))) {
                /* for round robin threads, there are only two refills: the HEAD, which is what
                 * we are consuming, and the tail, which is what we have consumed */
                assert(refill_size(NODE_STATE(ksCurSC)) == MIN_REFILLS);
                REFILL_HEAD(NODE_STATE(ksCurSC)).rAmount -= NODE_STATE(ksConsumed);
                REFILL_TAIL(NODE_STATE(ksCurSC)).rAmount += NODE_STATE(ksConsumed);
            } else {
                refill_split_check(NODE_STATE(ksConsumed));
            }
            assert(refill_sufficient(NODE_STATE(ksCurSC), 0));
            assert(refill_ready(NODE_STATE(ksCurSC)));
        }
        NODE_STATE(ksCurSC)->scConsumed += NODE_STATE(ksConsumed);
    }
    if (CONFIG_NUM_DOMAINS > 1) {
        assert(ksDomainTime > NODE_STATE(ksConsumed));
        assert(ksDomainTime - NODE_STATE(ksConsumed) >= MIN_BUDGET);
        ksDomainTime -= NODE_STATE(ksConsumed);
    }

    NODE_STATE(ksConsumed) = 0llu;
}

static inline bool_t PURE isSchedulable(const tcb_t *thread)
{
    return isRunnable(thread) &&
           thread->tcbSchedContext != NULL &&
           thread->tcbSchedContext->scRefillMax > 0 &&
           !thread_state_get_tcbInReleaseQueue(thread->tcbState);
}
#else
#define isSchedulable isRunnable
#endif

void configureIdleThread(tcb_t *tcb);
void activateThread(void);
void suspend(tcb_t *target);
void restart(tcb_t *target);
void doIPCTransfer(tcb_t *sender, endpoint_t *endpoint,
                   word_t badge, bool_t grant, tcb_t *receiver);
#ifdef CONFIG_KERNEL_MCS
void doReplyTransfer(tcb_t *sender, reply_t *reply, bool_t grant);
#else
void doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot, bool_t grant);
void timerTick(void);
#endif
void doNormalTransfer(tcb_t *sender, word_t *sendBuffer, endpoint_t *endpoint,
                      word_t badge, bool_t canGrant, tcb_t *receiver,
                      word_t *receiveBuffer);
void doFaultTransfer(word_t badge, tcb_t *sender, tcb_t *receiver,
                     word_t *receiverIPCBuffer);
void doNBRecvFailedTransfer(tcb_t *thread);
void schedule(void);
void chooseThread(void);
void switchToThread(tcb_t *thread);
void switchToIdleThread(void);
void setDomain(tcb_t *tptr, dom_t dom);
void setPriority(tcb_t *tptr, prio_t prio);
void setMCPriority(tcb_t *tptr, prio_t mcp);
void scheduleTCB(tcb_t *tptr);
void possibleSwitchTo(tcb_t *tptr);
void setThreadState(tcb_t *tptr, _thread_state_t ts);
void rescheduleRequired(void);

/* declare that the thread has had its registers (in its user_context_t) modified and it
 * should ignore any 'efficient' restores next time it is run, and instead restore all
 * registers into their correct place */
void Arch_postModifyRegisters(tcb_t *tptr);

/* Updates a threads FaultIP to match its NextIP. This is used to indicate that a
 * thread has completed its fault and by updating the restartPC means that if the thread
 * should get restarted in the future for any reason it is restart in such a way as to
 * not cause the fault again. */
static inline void updateRestartPC(tcb_t *tcb)
{
    setRegister(tcb, FaultIP, getRegister(tcb, NextIP));
}

#ifdef CONFIG_KERNEL_MCS
/* End the timeslice for the current thread.
 * This will recharge the threads timeslice and place it at the
 * end of the scheduling queue for its priority.
 */
void endTimeslice(bool_t can_timeout_fault);

/* called when a thread has used up its head refill */
void chargeBudget(ticks_t capacity, ticks_t consumed, bool_t canTimeoutFault, word_t core, bool_t isCurCPU);

/* Update the kernels timestamp and stores in ksCurTime.
 * The difference between the previous kernel timestamp and the one just read
 * is stored in ksConsumed.
 *
 * Should be called on every kernel entry
 * where threads can be billed.
 */
static inline void updateTimestamp(void)
{
    time_t prev = NODE_STATE(ksCurTime);
    NODE_STATE(ksCurTime) = getCurrentTime();
    NODE_STATE(ksConsumed) += (NODE_STATE(ksCurTime) - prev);
}

/* Check if the current thread/domain budget has expired.
 * if it has, bill the thread, add it to the scheduler and
 * set up a reschedule.
 *
 * @return true if the thread/domain has enough budget to
 *              get through the current kernel operation.
 */
static inline bool_t checkBudget(void)
{
    /* currently running thread must have available capacity */
    assert(refill_ready(NODE_STATE(ksCurSC)));

    ticks_t capacity = refill_capacity(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed));
    /* if the budget isn't enough, the timeslice for this SC is over. For
     * round robin threads this is sufficient, however for periodic threads
     * we also need to check there is space to schedule the replenishment - if the refill
     * is full then the timeslice is also over as the rest of the budget is forfeit. */
    if (likely(capacity >= MIN_BUDGET && (isRoundRobin(NODE_STATE(ksCurSC)) ||
                                          !refill_full(NODE_STATE(ksCurSC))))) {
        if (unlikely(isCurDomainExpired())) {
            NODE_STATE(ksReprogram) = true;
            rescheduleRequired();
            return false;
        }
        return true;
    }

    chargeBudget(capacity, NODE_STATE(ksConsumed), true, CURRENT_CPU_INDEX(), true);
    return false;
}

/* Everything checkBudget does, but also set the thread
 * state to ThreadState_Restart. To be called from kernel entries
 * where the operation should be restarted once the current thread
 * has budget again.
 */

static inline bool_t checkBudgetRestart(void)
{
    assert(isRunnable(NODE_STATE(ksCurThread)));
    bool_t result = checkBudget();
    if (!result && isRunnable(NODE_STATE(ksCurThread))) {
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    }
    return result;
}


/* Set the next kernel tick, which is either the end of the current
 * domains timeslice OR the end of the current threads timeslice.
 */
void setNextInterrupt(void);

/* Wake any periodic threads that are ready for budget recharge */
void awaken(void);
/* Place the thread bound to this scheduling context in the release queue
 * of periodic threads waiting for budget recharge */
void postpone(sched_context_t *sc);
#endif

