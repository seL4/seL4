/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <object.h>
#include <util.h>
#include <api/faults.h>
#include <api/types.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <model/statedata.h>
#include <arch/machine.h>
#include <arch/kernel/thread.h>
#include <machine/registerset.h>
#include <arch/linker.h>

static seL4_MessageInfo_t
transferCaps(seL4_MessageInfo_t info, extra_caps_t caps,
             endpoint_t *endpoint, tcb_t *receiver,
             word_t *receiveBuffer, bool_t diminish);

static inline bool_t PURE
isBlocked(const tcb_t *thread)
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

static inline bool_t PURE
isRunnable(const tcb_t *thread)
{
    switch (thread_state_get_tsType(thread->tcbState)) {
    case ThreadState_Running:
    case ThreadState_Restart:
        return true;

    default:
        return false;
    }
}

static inline bool_t PURE
isSchedulable(const tcb_t *thread)
{
    return isRunnable(thread) && thread->tcbSchedContext != NULL &&
           !thread_state_get_inReleaseQueue(thread->tcbState);
}

BOOT_CODE void
configureIdleThread(tcb_t *tcb)
{
    Arch_configureIdleThread(tcb);
    setThreadState(tcb, ThreadState_IdleThreadState);
}

void
activateThread(void)
{
    switch (thread_state_get_tsType(ksCurThread->tcbState)) {
    case ThreadState_Running:
        break;

    case ThreadState_Restart: {
        word_t pc;

        pc = getRestartPC(ksCurThread);
        setNextPC(ksCurThread, pc);
        setThreadState(ksCurThread, ThreadState_Running);
        break;
    }

    case ThreadState_IdleThreadState:
        Arch_activateIdleThread(ksCurThread);
        break;

    default:
        fail("Current thread is blocked");
    }
}

void
suspend(tcb_t *target)
{
    cancelIPC(target);
    setThreadState(target, ThreadState_Inactive);
    tcbSchedDequeue(target);
    tcbReleaseRemove(target);
}

void
restart(tcb_t *target)
{
    if (isBlocked(target)) {
        cancelIPC(target);
        setupReplyMaster(target);
        setThreadState(target, ThreadState_Restart);

        if (likely(target->tcbSchedContext) && target->tcbSchedContext->scBudget > 0) {
            /* recharge sched context */
            if (ready(target->tcbSchedContext)) {
                recharge(target->tcbSchedContext);
            }

            if (target->tcbSchedContext->scRemaining < getKernelWcetTicks()) {
                /* not enough budget */
                postpone(target->tcbSchedContext);
            } else {
                /* ready to go */
                tcbSchedEnqueue(target);
                switchIfRequiredTo(target);
            }
        }
    }
}

void
doIPCTransfer(tcb_t *sender, endpoint_t *endpoint, word_t badge,
              bool_t grant, tcb_t *receiver, bool_t diminish)
{
    void *receiveBuffer, *sendBuffer;

    receiveBuffer = lookupIPCBuffer(true, receiver);

    if (likely(!fault_get_faultType(sender->tcbFault) != fault_null_fault)) {
        sendBuffer = lookupIPCBuffer(false, sender);
        doNormalTransfer(sender, sendBuffer, endpoint, badge, grant,
                         receiver, receiveBuffer, diminish);
    } else {
        doFaultTransfer(badge, sender, receiver, receiveBuffer);
    }
}

void
doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot, sched_context_t *reply_sc)
{

    assert(thread_state_get_tsType(receiver->tcbState) ==
           ThreadState_BlockedOnReply);

    assert(reply_sc == NULL || reply_sc->scRemaining > getKernelWcetTicks());

    if (likely(reply_sc != NULL && receiver->tcbSchedContext == NULL)) {
        donateSchedContext(receiver, reply_sc);
        reply_sc->scReply = NULL;
    }

    if (likely(fault_get_faultType(receiver->tcbFault) == fault_null_fault)) {
        doIPCTransfer(sender, NULL, 0, true, receiver, false);
        /** GHOSTUPD: "(True, gs_set_assn cteDeleteOne_'proc (ucast cap_reply_cap))" */
        cteDeleteOne(slot);
        setThreadState(receiver, ThreadState_Running);
        attemptSwitchTo(receiver);
    } else {
        bool_t restart;

        /** GHOSTUPD: "(True, gs_set_assn cteDeleteOne_'proc (ucast cap_reply_cap))" */
        cteDeleteOne(slot);
        restart = handleFaultReply(receiver, sender);
        fault_null_fault_ptr_new(&receiver->tcbFault);
        if (restart) {
            setThreadState(receiver, ThreadState_Restart);
            attemptSwitchTo(receiver);
        } else {
            setThreadState(receiver, ThreadState_Inactive);
        }
    }
}

void
doNormalTransfer(tcb_t *sender, word_t *sendBuffer, endpoint_t *endpoint,
                 word_t badge, bool_t canGrant, tcb_t *receiver,
                 word_t *receiveBuffer, bool_t diminish)
{
    word_t msgTransferred;
    seL4_MessageInfo_t tag;
    exception_t status;
    extra_caps_t caps;

    tag = messageInfoFromWord(getRegister(sender, msgInfoRegister));

    if (canGrant) {
        status = lookupExtraCaps(sender, sendBuffer, tag);
        caps = current_extra_caps;
        if (unlikely(status != EXCEPTION_NONE)) {
            caps.excaprefs[0] = NULL;
        }
    } else {
        caps = current_extra_caps;
        caps.excaprefs[0] = NULL;
    }

    msgTransferred = copyMRs(sender, sendBuffer, receiver, receiveBuffer,
                             seL4_MessageInfo_get_length(tag));

    tag = transferCaps(tag, caps, endpoint, receiver, receiveBuffer, diminish);

    tag = seL4_MessageInfo_set_length(tag, msgTransferred);
    setRegister(receiver, msgInfoRegister, wordFromMessageInfo(tag));
    setRegister(receiver, badgeRegister, badge);
}

void
doFaultTransfer(word_t badge, tcb_t *sender, tcb_t *receiver,
                word_t *receiverIPCBuffer)
{
    word_t sent;
    seL4_MessageInfo_t msgInfo;

    sent = setMRs_fault(sender, receiver, receiverIPCBuffer);
    msgInfo = seL4_MessageInfo_new(
                  fault_get_faultType(sender->tcbFault), 0, 0, sent);
    setRegister(receiver, msgInfoRegister, wordFromMessageInfo(msgInfo));
    setRegister(receiver, badgeRegister, badge);
}

/* Like getReceiveSlots, this is specialised for single-cap transfer. */
static seL4_MessageInfo_t
transferCaps(seL4_MessageInfo_t info, extra_caps_t caps,
             endpoint_t *endpoint, tcb_t *receiver,
             word_t *receiveBuffer, bool_t diminish)
{
    word_t i;
    cte_t* destSlot;

    info = seL4_MessageInfo_set_extraCaps(info, 0);
    info = seL4_MessageInfo_set_capsUnwrapped(info, 0);

    if (likely(!caps.excaprefs[0] || !receiveBuffer)) {
        return info;
    }

    destSlot = getReceiveSlots(receiver, receiveBuffer);

    for (i = 0; i < seL4_MsgMaxExtraCaps && caps.excaprefs[i] != NULL; i++) {
        cte_t *slot = caps.excaprefs[i];
        cap_t cap = slot->cap;

        if (cap_get_capType(cap) == cap_endpoint_cap &&
                EP_PTR(cap_endpoint_cap_get_capEPPtr(cap)) == endpoint) {
            /* If this is a cap to the endpoint on which the message was sent,
             * only transfer the badge, not the cap. */
            setExtraBadge(receiveBuffer,
                          cap_endpoint_cap_get_capEPBadge(cap), i);

            info = seL4_MessageInfo_set_capsUnwrapped(info,
                                                      seL4_MessageInfo_get_capsUnwrapped(info) | (1 << i));

        } else {
            deriveCap_ret_t dc_ret;

            if (!destSlot) {
                break;
            }

            if (diminish) {
                dc_ret = deriveCap(slot, maskCapRights(noWrite, cap));
            } else {
                dc_ret = deriveCap(slot, cap);
            }

            if (dc_ret.status != EXCEPTION_NONE) {
                break;
            }
            if (cap_get_capType(dc_ret.cap) == cap_null_cap) {
                break;
            }

            cteInsert(dc_ret.cap, slot, destSlot);

            destSlot = NULL;
        }
    }

    return seL4_MessageInfo_set_extraCaps(info, i);
}

void doNBRecvFailedTransfer(tcb_t *thread)
{
    /* Set the badge register to 0 to indicate there was no message */
    setRegister(thread, badgeRegister, 0);
}

static void
setNextInterrupt(void)
{
    /* the next interrupt is when the current thread's budget expires */
    time_t next_interrupt = ksCurrentTime + ksCurThread->tcbSchedContext->scRemaining;

    /* or when the next thread in the release queue is due to awaken */
    if (likely(ksReleaseHead != NULL &&
               ksReleaseHead->tcbSchedContext->scNext < next_interrupt)) {
        next_interrupt = ksReleaseHead->tcbSchedContext->scNext;
    }

    setDeadline(next_interrupt - getTimerPrecision());
}

void
schedule(void)
{
    word_t action;

    if (ksReprogram) {
        /* wake any threads that need it */
        awaken();
    }

    action = (word_t)ksSchedulerAction;
    if (action == (word_t)SchedulerAction_ChooseNewThread) {
        if (isSchedulable(ksCurThread)) {
            tcbSchedEnqueue(ksCurThread);
        }
        chooseThread();
        ksSchedulerAction = SchedulerAction_ResumeCurrentThread;
    } else if (action != (word_t)SchedulerAction_ResumeCurrentThread) {
        if (isSchedulable(ksCurThread)) {
            tcbSchedEnqueue(ksCurThread);
        }
        /* SwitchToThread */
        switchToThread(ksSchedulerAction);
        ksSchedulerAction = SchedulerAction_ResumeCurrentThread;
    }

    switchSchedContext();

    if (ksReprogram) {
        setNextInterrupt();
        ksReprogram = false;
    }
    assert(ksConsumed == 0);

    /* ksCurThread is implicitly bound to ksCurSchedContext -
     * this optimised scheduling context donation on the fastpath
     */
    ksCurThread->tcbSchedContext = NULL;
    ksCurSchedContext->scTcb = NULL;
}

void
chooseThread(void)
{
    word_t prio;
    tcb_t *thread;

    if (likely(ksReadyQueuesL1Bitmap)) {
        prio = highestPrio();
        thread = ksReadyQueues[prio].head;
        assert(thread);
        assert(isRunnable(thread));
        assert(isSchedulable(thread));
        assert(thread->tcbSchedContext->scRemaining >= getKernelWcetTicks());
        switchToThread(thread);
    } else {
        switchToIdleThread();
    }
}

void
switchToThread(tcb_t *thread)
{
    /* now switch */
    if (likely(thread != ksCurThread)) {
        Arch_switchToThread(thread);
    }
    tcbSchedDequeue(thread);
    assert(!thread_state_get_inReleaseQueue(thread->tcbState));
    assert(thread->tcbSchedContext->scRemaining >= getKernelWcetTicks());
    ksCurThread = thread;
}

void
switchToIdleThread(void)
{
    /* make sure the calculation in setNextInterrupt doesn't overflow for the idle thread */
    ksIdleThread->tcbSchedContext->scRemaining = UINT64_MAX - ksCurrentTime - 1;
    Arch_switchToIdleThread();
    ksCurThread = ksIdleThread;
}

void
switchSchedContext(void)
{

    assert(ksCurSchedContext->scRemaining >= ksConsumed);

    if (unlikely(ksCurSchedContext != ksCurThread->tcbSchedContext)) {
        /* we are changing scheduling contexts */
        ksReprogram = true;
        ksCurSchedContext->scRemaining -= ksConsumed;
        ksConsumed = 0llu;
        ksCurrentTime += 1llu;
    } else {
        /* go back in time and skip reprogramming the timer */
        ksCurrentTime -= ksConsumed;
        ksConsumed = 0llu;
    }
    ksCurSchedContext = ksCurThread->tcbSchedContext;
}

void
setPriority(tcb_t *tptr, seL4_Prio_t new_prio)
{
    prio_t old_prio = tptr->tcbPriority;

    /* lazy dequeue */
    tcbSchedDequeue(tptr);

    tptr->tcbPriority = seL4_Prio_get_prio(new_prio);
    tptr->tcbMaxPriority = seL4_Prio_get_max_prio(new_prio);

    switch (thread_state_ptr_get_tsType(&tptr->tcbState)) {
    case ThreadState_BlockedOnReceive:
    case ThreadState_BlockedOnSend:
        reorderIPCQueue(tptr, old_prio);
        break;
    case ThreadState_BlockedOnNotification:
        reorderNtfnQueue(tptr, old_prio);
        break;
    case ThreadState_Running:
    case ThreadState_Restart:
        if (!thread_state_get_inReleaseQueue(tptr->tcbState)) {
            tcbSchedEnqueue(tptr);
            if (tptr == ksCurThread) {
                rescheduleRequired();
            }
        }
        break;
    default:
        /* nothing to do - thread is inactive, idle or blocked on reply */
        break;
    }
}

void
donateSchedContext(tcb_t *to, sched_context_t *sc)
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

static void
possibleSwitchTo(tcb_t* target, bool_t onSamePriority)
{
    prio_t curPrio, targetPrio;
    tcb_t *action;

    curPrio = ksCurThread->tcbPriority;
    targetPrio = target->tcbPriority;
    action = ksSchedulerAction;

    if (unlikely(target->tcbSchedContext != NULL)) {
        if ((targetPrio > curPrio || (targetPrio == curPrio && onSamePriority))
                && action == SchedulerAction_ResumeCurrentThread) {
            ksSchedulerAction = target;
        } else {
            tcbSchedEnqueue(target);
        }

        if (action != SchedulerAction_ResumeCurrentThread &&
                action != SchedulerAction_ChooseNewThread) {
            rescheduleRequired();
        } else if (ksCurThread->tcbSchedContext == NULL &&
                   ksSchedulerAction == SchedulerAction_ResumeCurrentThread) {
            rescheduleRequired();
        }
    }
}

void
attemptSwitchTo(tcb_t* target)
{
    possibleSwitchTo(target, true);
}

void
switchIfRequiredTo(tcb_t* target)
{
    possibleSwitchTo(target, false);
}

void
setThreadState(tcb_t *tptr, _thread_state_t ts)
{
    thread_state_ptr_set_tsType(&tptr->tcbState, ts);
    scheduleTCB(tptr);
}

void
scheduleTCB(tcb_t *tptr)
{
    if (tptr == ksCurThread &&
            ksSchedulerAction == SchedulerAction_ResumeCurrentThread &&
            !isRunnable(tptr)) {
        rescheduleRequired();
    }
}

void
recharge(sched_context_t *sc)
{
    sc->scRemaining = sc->scBudget;
    assert(sc->scBudget > 0);
    sc->scNext = ksCurrentTime + sc->scPeriod;
}

void
postpone(sched_context_t *sc)
{
    /* this isn't technically neccessary however
     * we don't want to leave threads with budget when they
     * have used / abadondoned it as various assertions around
     * the kernel will cease to guard if this is not 0
     */
    sc->scRemaining = 0llu;
    tcbReleaseEnqueue(sc->scTcb);
}

/* update the kernel timestamp and store much
 * time we have consumed since the last update
 */
void
updateTimestamp(void)
{
    time_t prev = ksCurrentTime;
    ksCurrentTime = getCurrentTime();
    ksConsumed = ksCurrentTime - prev;

    /* restore sched context binding */
    ksCurThread->tcbSchedContext = ksCurSchedContext;
    ksCurSchedContext->scTcb = ksCurThread;
}

/*
 * Check if the current threads budget has expired.
 *
 * If it has, bill the thread, add it to the scheduler
 * and set up a reschedule.
 *
 * return true if the thread has enough budget to get through
 *             the current kernel operation.
 */
bool_t
checkBudget(void)
{
    /* does this thread have enough time to continue? */
    if (unlikely(currentThreadExpired())) {
        /* since we never bill the idle thread this shouldn't ever happen */
        assert(ksCurThread != ksIdleThread);

        /* we enter this function on 2 different types of path:
         * kernel entry (for whatever reason) and when handling
         * a timer interrupt. For the
         * former, all threads should be runnable on kernel entry.
         * For the latter, all threads being preempted are currently
         * doing something so they should be running
         */
        assert(isRunnable(ksCurThread));

        ksConsumed = 0u;
        endTimeslice(ksCurThread->tcbSchedContext);

        /* consumed time has been billed */
        rescheduleRequired();
        return false;
    } else {
        /* thread is good to go to do whatever it is up to */
        return true;
    }
}

void
endTimeslice(sched_context_t *sc)
{
    assert(sc->scTcb != NULL);
    assert(isSchedulable(sc->scTcb));

    tcbSchedDequeue(sc->scTcb);

    if (ready(sc)) {
        /* refill budget */
        recharge(sc);
        /* apply round robin */
        tcbSchedAppend(sc->scTcb);
    } else {
        /* schedule thread to wake up when budget is due to be recharged */
        postpone(sc);
    }
}

void
rescheduleRequired(void)
{
    if (ksSchedulerAction != SchedulerAction_ResumeCurrentThread
            && ksSchedulerAction != SchedulerAction_ChooseNewThread
            && isSchedulable(ksSchedulerAction)) {

        tcbSchedEnqueue(ksSchedulerAction);
    }
    ksSchedulerAction = SchedulerAction_ChooseNewThread;
    ksReprogram = true;
}

/* wake any threads in the release queue that are ready to
 * have their budgets replenished
 */
void
awaken(void)
{
    tcb_t *awakened;

    while (ksReleaseHead != NULL && ready(ksReleaseHead->tcbSchedContext)) {
        awakened = tcbReleaseDequeue();
        recharge(awakened->tcbSchedContext);
        tcbSchedAppend(awakened);
        rescheduleRequired();
    }
}

