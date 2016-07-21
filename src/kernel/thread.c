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

/* We set bits 8 -- 15 to the current criticality, which is guaranteed to be higher than
 * any lower criticality tcbs. First clear any bits previously adjusted by criticality,
 * then  boost priority by new criticality
 */
#define SHIFT_PRIO_BY_CRIT(p) ((p  & 0xFF) | (ksCriticality << 8u))

static seL4_MessageInfo_t
transferCaps(seL4_MessageInfo_t info, extra_caps_t caps,
             endpoint_t *endpoint, tcb_t *receiver,
             word_t *receiveBuffer);

static inline bool_t PURE
isBlocked(const tcb_t *thread)
{
    return thread_state_get_tsType(thread->tcbState) < ThreadState_Running;
}

BOOT_CODE void
configureIdleThread(tcb_t *tcb)
{
    Arch_configureIdleThread(tcb);
    setThreadState(tcb, ThreadState_Running);
    tcb->tcbPriority = seL4_MinPrio;
    tcb->tcbCrit     = seL4_MaxCrit;
}

void
activateThread(void)
{
    uint32_t state;

    assert(isRunnable(ksCurThread));

    state = thread_state_get_tsType(ksCurThread->tcbState);
    if (unlikely(state == ThreadState_YieldTo)) {
        schedContext_completeYieldTo(ksCurThread);
    } else if (unlikely(state == ThreadState_Restart)) {
        word_t pc;

        pc = getRestartPC(ksCurThread);
        setNextPC(ksCurThread, pc);
        setThreadState(ksCurThread, ThreadState_Running);
    }
}

void
suspend(tcb_t *target)
{
    cancelIPC(target);
    setThreadState(target, ThreadState_Inactive);
    tcbSchedDequeue(target);
    tcbReleaseRemove(target);
    tcbCritDequeue(target);

    /* like IPC, yieldTo is cancelled by suspension */
    if (target->tcbYieldTo) {
        target->tcbYieldTo->scYieldFrom = NULL;
        target->tcbYieldTo = NULL;
    }
}

void
restart(tcb_t *target)
{
    if (isBlocked(target)) {
        cancelIPC(target);
        setupReplyMaster(target);
        setThreadState(target, ThreadState_Restart);
        tcbCritEnqueue(target);

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
              bool_t grant, tcb_t *receiver)
{
    void *receiveBuffer, *sendBuffer;

    receiveBuffer = lookupIPCBuffer(true, receiver);

    if (likely(!seL4_Fault_get_seL4_FaultType(sender->tcbFault) != seL4_Fault_NullFault)) {
        sendBuffer = lookupIPCBuffer(false, sender);
        doNormalTransfer(sender, sendBuffer, endpoint, badge, grant,
                         receiver, receiveBuffer);
    } else {
        doFaultTransfer(badge, sender, receiver, receiveBuffer);
    }
}

void
doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot)
{

    assert(thread_state_get_tsType(receiver->tcbState) ==
           ThreadState_BlockedOnReply);

    /* if the call stack is set then the receiver donated a scheduling context
     * over the reply cap, work out if we can return it */
    if (likely(receiver->tcbCallStackNext && receiver->tcbSchedContext == NULL)) {
        if (unlikely(receiver->tcbCallStackNext != sender)) {
            /* we are replying on behalf of someone else */
            if (receiver->tcbCallStackNext->tcbSchedContext) {
                /* return the donated scheduling context back, but don't pop the
                 * call stack as we don't know what it looks like - instead the
                 * appropriate thread will be removed when the reply cap is cleaned up*/
                schedContext_donate(receiver, receiver->tcbCallStackNext->tcbSchedContext);
            } else if (sender->tcbSchedContext->scHome != sender) {
                /* otherwise someone else is replying, if the sender doesn't hold its own
                 * scheduling context, send it back to the receiver */
                schedContext_donate(receiver, sender->tcbSchedContext);
            }
        } else {
            /* this is a reply from the callee, send the scheduling context back */
            schedContext_donate(receiver, sender->tcbSchedContext);
            tcbCallStackPop(sender);
        }
    }

    /* since the callee wasn't running, the scheduling context may not be active */
    if (ready(receiver->tcbSchedContext)) {
        recharge(receiver->tcbSchedContext);
    }

    if (receiver->tcbSchedContext &&
            receiver->tcbSchedContext->scRemaining <= getKernelWcetTicks()) {
        /* thread still has not enough budget to run */
        cap_t tfep = getTemporalFaultHandler(receiver);
        if (validTemporalFaultHandler(tfep) &&
                seL4_Fault_get_seL4_FaultType(receiver->tcbFault) != seL4_Fault_Temporal) {
            /* the context does not have enough budget and the thread we are
             * switching to has a temporal fault handler, raise a temporal
             * fault and abort the reply */
            cteDeleteOne(slot);
            current_fault = seL4_Fault_Temporal_new(receiver->tcbSchedContext->scData);
            sendTemporalFaultIPC(receiver, tfep);
            return;
        } else {
            /* the context doesn't have enough budget, but no temporal fault handler,
            * just postpone it and continue to process the reply. The thread will
            * pick it up once the scheduling context has its budget replenished.
            */
            postpone(receiver->tcbSchedContext);
        }
    }

    if (likely(seL4_Fault_get_seL4_FaultType(receiver->tcbFault) == seL4_Fault_NullFault)) {
        doIPCTransfer(sender, NULL, 0, true, receiver);
        /** GHOSTUPD: "(True, gs_set_assn cteDeleteOne_'proc (ucast cap_reply_cap))" */
        cteDeleteOne(slot);
        setThreadState(receiver, ThreadState_Running);
        attemptSwitchTo(receiver);
    } else {
        bool_t restart;

        /** GHOSTUPD: "(True, gs_set_assn cteDeleteOne_'proc (ucast cap_reply_cap))" */
        cteDeleteOne(slot);
        restart = handleFaultReply(receiver, sender);
        seL4_Fault_NullFault_ptr_new(&receiver->tcbFault);
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
                 word_t *receiveBuffer)
{
    word_t msgTransferred;
    seL4_MessageInfo_t tag;
    word_t length;
    exception_t status;
    extra_caps_t caps;

    tag = messageInfoFromWord(getRegister(sender, msgInfoRegister));
    length = seL4_MessageInfo_get_extraCaps(tag);

    if (unlikely(length > 0 && canGrant)) {
        status = lookupExtraCaps(sender, sendBuffer, length);
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

    if (unlikely(length > 0)) {
        tag = transferCaps(tag, caps, endpoint, receiver, receiveBuffer);
    }

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
                  seL4_Fault_get_seL4_FaultType(sender->tcbFault), 0, 0, sent);
    setRegister(receiver, msgInfoRegister, wordFromMessageInfo(msgInfo));
    setRegister(receiver, badgeRegister, badge);
}

/* Like getReceiveSlots, this is specialised for single-cap transfer. */
static seL4_MessageInfo_t
transferCaps(seL4_MessageInfo_t info, extra_caps_t caps,
             endpoint_t *endpoint, tcb_t *receiver,
             word_t *receiveBuffer)
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

            dc_ret = deriveCap(slot, cap);

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

    prio = highestPrio();
    thread = ksReadyQueues[prio].head;

    if (unlikely(thread->tcbCrit < ksCriticality)) {
        thread = ksIdleThread;
    }

    assert(thread);
    assert(isRunnable(thread));
    assert(isSchedulable(thread));
    assert(thread->tcbSchedContext->scRemaining >= getKernelWcetTicks());

    switchToThread(thread);
}

void
switchToThread(tcb_t *thread)
{
    /* now switch */
    if (unlikely(thread == ksIdleThread)) {
        Arch_switchToIdleThread();
    } else if (likely(thread != ksCurThread)) {
        Arch_switchToThread(thread);
    }

    tcbSchedDequeue(thread);
    assert(!thread_state_get_tcbInReleaseQueue(thread->tcbState));
    assert(thread->tcbSchedContext->scRemaining >= getKernelWcetTicks());

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(ksCurThread, thread);
#endif

    ksCurThread = thread;
}

void
switchSchedContext(void)
{
    assert(ksCurSchedContext->scRemaining >= ksConsumed);

    if (unlikely(ksCurSchedContext != ksCurThread->tcbSchedContext)) {
        /* we are changing scheduling contexts */
        ksReprogram = true;
        commitTime(ksCurSchedContext);
    } else {
        /* go back in time and skip reprogramming the timer */
        rollbackTime();
    }
    ksCurSchedContext = ksCurThread->tcbSchedContext;
}

void
setActivePriority(tcb_t *tptr, prio_t prio)
{
    prio_t old_prio = tptr->tcbPriority;

    if (old_prio == prio) {
        return;
    }

    /* lazy dequeue */
    tcbSchedDequeue(tptr);
    /* update prio */
    tptr->tcbPriority = prio;

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
        if (!thread_state_get_tcbInReleaseQueue(tptr->tcbState)) {
            if (tptr->tcbSchedContext) {
                tcbSchedEnqueue(tptr);
            }
            if (tptr == ksCurThread || prio > ksCurThread->tcbPriority) {
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
setPriorityFields(tcb_t *tptr, seL4_Prio_t new_prio)
{
    prio_t activePrio;

    /* lazy dequeue */
    tcbCritDequeue(tptr);

    tptr->tcbMCP = seL4_Prio_get_mcp(new_prio);
    tptr->tcbCrit = seL4_Prio_get_crit(new_prio);
    tptr->tcbMCC = seL4_Prio_get_mcc(new_prio);

    activePrio = seL4_Prio_get_prio(new_prio);
    if (tptr->tcbCrit >= ksCriticality) {
        activePrio = SHIFT_PRIO_BY_CRIT(activePrio);
    }

    setActivePriority(tptr, activePrio);

    if (thread_state_get_tsType(tptr->tcbState) != ThreadState_Inactive) {
        tcbCritEnqueue(tptr);
    }
}

static void
possibleSwitchTo(tcb_t* target, bool_t onSamePriority)
{
    prio_t curPrio, targetPrio;
    tcb_t *action;

    curPrio = ksCurThread->tcbPriority;
    targetPrio = target->tcbPriority;
    action = ksSchedulerAction;

    if (likely(target->tcbSchedContext != NULL) &&
            !thread_state_get_tcbInReleaseQueue(target->tcbState)) {
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
    } else {
        rescheduleRequired();
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
        cap_t tfep = getTemporalFaultHandler(ksCurThread);
        /* we enter this function on 2 different types of path:
         * kernel entry (for whatever reason) and when handling
         * a timer interrupt. For the
         * former, all threads should be runnable on kernel entry.
         * For the latter, all threads being preempted are currently
         * doing something so they should be running
         */
        assert(isRunnable(ksCurThread));
        commitTime(ksCurSchedContext);
        if (validTemporalFaultHandler(tfep)) {
            /* threads with temporal fault handlers should never run out of budget! */
            current_fault = seL4_Fault_Temporal_new(ksCurSchedContext->scData);
            sendTemporalFaultIPC(ksCurThread, tfep);
        } else {
            /* threads without fault handlers don't care if they ran out of budget */
            endTimeslice(ksCurThread->tcbSchedContext);
            rescheduleRequired();
        }

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
    rescheduleRequired();
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
        switchIfRequiredTo(awakened);
    }
}

void
adjustPriorityByCriticality(tcb_t *tcb, bool_t up)
{
    /* this function should only be called on threads that are currently eligible to run */
    assert(tcb->tcbCrit >= ksCriticality);
    assert(tcb->tcbCrit > 0);

    if (up &&
            isSchedulable(tcb) &&
            tcb->tcbSchedContext->scHome != tcb &&
            tcb->tcbSchedContext->scHome->tcbCrit < ksCriticality) {

        /* if we are adjusting the criticality up, we need to
         * raise a temporal exception if a server is working
         * on a request for a low criticality thread */
        cap_t tfep = getTemporalFaultHandler(tcb);
        if (validTemporalFaultHandler(tfep)) {
            current_fault = seL4_Fault_Temporal_new(tcb->tcbSchedContext->scData);
            sendTemporalFaultIPC(tcb, tfep);
        }
    }

    setActivePriority(tcb, SHIFT_PRIO_BY_CRIT(tcb->tcbPriority));
}


