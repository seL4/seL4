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
#include <kernel/sporadic.h>
#include <machine/timer.h>
#include <mode/machine.h>

static inline CONST word_t
ready_queues_index(word_t dom, word_t prio)
{
    if (CONFIG_NUM_DOMAINS > 1) {
        return dom * CONFIG_NUM_PRIORITIES + prio;
    } else {
        assert(dom == 0);
        return prio;
    }
}

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

static inline bool_t PURE
isRunnable(const tcb_t *thread)
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

static inline bool_t
isCurDomainExpired(void)
{
    return CONFIG_NUM_DOMAINS > 1 &&
           ksDomainTime < (NODE_STATE(ksConsumed) + getKernelWcetTicks());
}

static inline void
commitTime(void)
{

    if (likely(NODE_STATE(ksConsumed) > 0 && (NODE_STATE(ksCurThread) != NODE_STATE(ksIdleThread)))) {
        assert(refill_sufficient(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed)));
        assert(refill_ready(NODE_STATE(ksCurSC)));
        refill_split_check(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed));
        assert(refill_sufficient(NODE_STATE(ksCurSC), 0));
        assert(refill_ready(NODE_STATE(ksCurSC)));
    }
    if (CONFIG_NUM_DOMAINS > 1) {
        if (unlikely(ksDomainTime < NODE_STATE(ksConsumed))) {
            ksDomainTime = 0;
        } else {
            ksDomainTime -= NODE_STATE(ksConsumed);
        }
    }

    NODE_STATE(ksConsumed) = 0llu;
}

static inline void
rollbackTime(void)
{
    NODE_STATE(ksCurTime) -= NODE_STATE(ksConsumed);
    NODE_STATE(ksConsumed) = 0llu;
}

void configureIdleThread(tcb_t *tcb);
void activateThread(void) VISIBLE;
void suspend(tcb_t *target);
void restart(tcb_t *target);
void doIPCTransfer(tcb_t *sender, endpoint_t *endpoint,
                   word_t badge, bool_t grant, tcb_t *receiver);
void doReplyTransfer(tcb_t *sender, reply_t *reply);
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
void setDomain(tcb_t *tptr, dom_t dom);
void setPriority(tcb_t *tptr, prio_t prio);
void setMCPriority(tcb_t *tptr, prio_t mcp);
void scheduleTCB(tcb_t *tptr);
void attemptSwitchTo(tcb_t *tptr);
void switchIfRequiredTo(tcb_t *tptr);
void setThreadState(tcb_t *tptr, _thread_state_t ts) VISIBLE;
void rescheduleRequired(void);

/* End the timeslice for the current thread.
 * This will recharge the threads timeslice and place it at the
 * end of the scheduling queue for its priority.
 */
void endTimeslice(void);

/* called when a thread has used up its head refill */
void chargeBudget(ticks_t capacity);

/* Update the kernels timestamp and stores in ksCurTime.
 * The difference between the previous kernel timestamp and the one just read
 * is stored in ksConsumed.
 *
 * Should be called on every kernel entry
 * where threads can be billed.
 *
 * @pre (NODE_STATE(ksConsumed) == 0
 */
static inline void
updateTimestamp(bool_t incrementConsumedTime)
{
    time_t prev = NODE_STATE(ksCurTime);
    NODE_STATE(ksCurTime) = getCurrentTime();
    if ((config_set(CONFIG_DEBUG_BUILD) || config_set(CONFIG_PRINTING))
        && incrementConsumedTime) {
        /* When executing debugging functions in the kernel that
         * increase the duration of a syscall, it's useful to call
         * updateTimestamp() in those debugging functions (such as printf).
         *
         * The reason we update the kernel timestamp is because if we don't,
         * the timestamp that will be used in setDeadline() will be very stale,
         * and the value programmed into the sched-timer will have considerable
         * skew.
         *
         * The standard case is below.
         */
        NODE_STATE(ksConsumed) += NODE_STATE(ksCurTime) - prev;
    } else {
        /* This is the standard case: we will usually want to track the
         * consumed time since the last call.
         */
        NODE_STATE(ksConsumed) = NODE_STATE(ksCurTime) - prev;
    }
}

/* Check if the current thread/domain budget has expired.
 * if it has, bill the thread, add it o the scheduler and
 * set up a reschedule.
 *
 * @return true if the thread/domain has enough budget to
 *              get through the current kernel operation.
 */
static inline bool_t
checkBudget(void)
{
    /* currently running thread must have available capacity */
    assert(refill_ready(NODE_STATE(ksCurSC)));

    if (unlikely(NODE_STATE(ksCurThread) == NODE_STATE(ksIdleThread))) {
        return true;
    }

    ticks_t capacity = refill_capacity(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed));
    if (likely(capacity >= MIN_BUDGET && (NODE_STATE(ksCurSC)->scPeriod == 0 ||
                    !refill_full(NODE_STATE(ksCurSC))))) {
        if (unlikely(isCurDomainExpired())) {
            commitTime();
            rescheduleRequired();
            return false;
        }
        return true;
    }

    chargeBudget(capacity);
    return false;
}

/* Everything checkBudget does, but also set the thread
 * state to ThreadState_Restart. To be called from kernel entries
 * where the operation should be restarted once the current thread
 * has budget again.
 */

static inline bool_t
checkBudgetRestart(void)
{
    assert(isRunnable(NODE_STATE(ksCurThread)));
    bool_t result = checkBudget();
    if (!result) {
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
