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
#include <object/schedcontext.h>
#include <model/statedata.h>
#include <arch/machine.h>
#include <arch/kernel/thread.h>
#include <machine/registerset.h>
#include <arch/linker.h>

static seL4_MessageInfo_t
transferCaps(seL4_MessageInfo_t info, extra_caps_t caps,
             endpoint_t *endpoint, tcb_t *receiver,
             word_t *receiveBuffer);

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
isSchedulable(const tcb_t *thread)
{
    return isRunnable(thread) &&
           thread->tcbSchedContext != NULL &&
           !thread_state_get_tcbInReleaseQueue(thread->tcbState);
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
    switch (thread_state_get_tsType(NODE_STATE(ksCurThread)->tcbState)) {
    case ThreadState_Running:
#ifdef CONFIG_VTX
    case ThreadState_RunningVM:
#endif
        break;

    case ThreadState_Restart: {
        word_t pc;

        pc = getRestartPC(NODE_STATE(ksCurThread));
        setNextPC(NODE_STATE(ksCurThread), pc);
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Running);
        break;
    }

    case ThreadState_IdleThreadState:
        Arch_activateIdleThread(NODE_STATE(ksCurThread));
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
        setThreadState(target, ThreadState_Restart);
        schedContext_resume(target->tcbSchedContext);
        switchIfRequiredTo(target);
    }
}

void
doIPCTransfer(tcb_t *sender, endpoint_t *endpoint, word_t badge,
              bool_t grant, tcb_t *receiver)
{
    void *receiveBuffer, *sendBuffer;

    receiveBuffer = lookupIPCBuffer(true, receiver);

    if (likely(seL4_Fault_get_seL4_FaultType(sender->tcbFault) == seL4_Fault_NullFault)) {
        sendBuffer = lookupIPCBuffer(false, sender);
        doNormalTransfer(sender, sendBuffer, endpoint, badge, grant,
                         receiver, receiveBuffer);
    } else {
        doFaultTransfer(badge, sender, receiver, receiveBuffer);
    }
}

void
doReplyTransfer(tcb_t *sender, reply_t *reply)
{
    if (unlikely(reply->replyCaller == NULL)) {
        return;
    }

    assert(thread_state_get_tsType(reply->replyCaller->tcbState) ==
           ThreadState_BlockedOnReply);

    tcb_t *receiver = reply->replyCaller;
    reply_remove(reply);
    thread_state_ptr_set_replyObject(&receiver->tcbState, REPLY_REF(0));

    if (likely(seL4_Fault_get_seL4_FaultType(receiver->tcbFault) == seL4_Fault_NullFault)) {
        doIPCTransfer(sender, NULL, 0, true, receiver);
        setThreadState(receiver, ThreadState_Running);
        attemptSwitchTo(receiver);
    } else {
        bool_t restart;

        restart = handleFaultReply(receiver, sender);
        receiver->tcbFault = seL4_Fault_NullFault_new();
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

    tag = transferCaps(tag, caps, endpoint, receiver, receiveBuffer);

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
nextDomain(void)
{
    ksDomScheduleIdx++;
    if (ksDomScheduleIdx >= ksDomScheduleLength) {
        ksDomScheduleIdx = 0;
    }
    NODE_STATE(ksReprogram) = true;
    ksWorkUnitsCompleted = 0;
    ksCurDomain = ksDomSchedule[ksDomScheduleIdx].domain;
    ksDomainTime = usToTicks(ksDomSchedule[ksDomScheduleIdx].length * US_IN_MS);
}

static void
switchSchedContext(void)
{
    if (unlikely(NODE_STATE(ksCurSC) != NODE_STATE(ksCurThread)->tcbSchedContext)) {
        NODE_STATE(ksReprogram) = true;
        refill_unblock_check(NODE_STATE(ksCurThread->tcbSchedContext));

        assert(refill_ready(NODE_STATE(ksCurThread->tcbSchedContext)));
        assert(refill_sufficient(NODE_STATE(ksCurThread->tcbSchedContext), 0));
    }

    if (NODE_STATE(ksReprogram)) {
        /* if we are reprogamming, we have acted on the new kernel time and cannot
         * rollback -> charge the current thread */
        commitTime();
    } else {
        /* otherwise, we don't need to do anything - avoid reprogramming the timer */
        rollbackTime();
    }

    /* if a thread doesn't have enough budget, it should not be in the scheduler */
    if (!refill_ready(NODE_STATE(ksCurSC)) || !refill_sufficient(NODE_STATE(ksCurSC), 0)) {
           assert(!thread_state_get_tcbQueued(NODE_STATE(ksCurSC)->scTcb->tcbState));
    }

    NODE_STATE(ksCurSC) = NODE_STATE(ksCurThread)->tcbSchedContext;
}

void
schedule(void)
{
    word_t action;

    awaken();

    action = (word_t)NODE_STATE(ksSchedulerAction);
    if (action == (word_t)SchedulerAction_ChooseNewThread) {
        if (isSchedulable(NODE_STATE(ksCurThread))) {
            SCHED_ENQUEUE_CURRENT_TCB;
        }
        if (ksDomainTime == 0) {
            nextDomain();
        }
        chooseThread();
        NODE_STATE(ksSchedulerAction) = SchedulerAction_ResumeCurrentThread;
    } else if (action != (word_t)SchedulerAction_ResumeCurrentThread) {
        if (isSchedulable(NODE_STATE(ksCurThread))) {
            SCHED_ENQUEUE_CURRENT_TCB;
        }
        /* SwitchToThread */
        switchToThread(NODE_STATE(ksSchedulerAction));
        NODE_STATE(ksSchedulerAction) = SchedulerAction_ResumeCurrentThread;
    }

#if CONFIG_MAX_NUM_NODES > 1
    doMaskReschedule(ARCH_NODE_STATE(ipiReschedulePending));
    ARCH_NODE_STATE(ipiReschedulePending) = 0;
#endif

    switchSchedContext();

    if (NODE_STATE(ksReprogram)) {
        setNextInterrupt();
        NODE_STATE(ksReprogram) = false;
    }
}

void
chooseThread(void)
{
    word_t prio;
    word_t dom;
    tcb_t *thread;

    if (CONFIG_NUM_DOMAINS > 1) {
        dom = ksCurDomain;
    } else {
        dom = 0;
    }

    if (likely(NODE_STATE(ksReadyQueuesL1Bitmap[dom]))) {
        word_t l1index = (wordBits - 1) - clzl(NODE_STATE(ksReadyQueuesL1Bitmap[dom]));
        word_t l2index = (wordBits - 1) - clzl(NODE_STATE(ksReadyQueuesL2Bitmap[dom][l1index]));
        prio = l1index_to_prio(l1index) | l2index;
        thread = NODE_STATE(ksReadyQueues[ready_queues_index(dom, prio)]).head;
        assert(thread);
        assert(isSchedulable(thread));
        assert(refill_sufficient(thread->tcbSchedContext, 0));
        assert(refill_ready(thread->tcbSchedContext));
        switchToThread(thread);
    } else {
        switchToIdleThread();
    }
}

void
switchToThread(tcb_t *thread)
{
    assert(thread->tcbSchedContext != NULL);
    assert(!thread_state_get_tcbInReleaseQueue(thread->tcbState));
    assert(refill_sufficient(thread->tcbSchedContext, 0));
    assert(refill_ready(thread->tcbSchedContext));

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(NODE_STATE(ksCurThread), thread);
#endif
    Arch_switchToThread(thread);
    tcbSchedDequeue(thread);
    NODE_STATE(ksCurThread) = thread;
}

void
switchToIdleThread(void)
{
#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_utilisation_switch(NODE_STATE(ksCurThread), NODE_STATE(ksIdleThread));
#endif
    Arch_switchToIdleThread();
    NODE_STATE(ksCurThread) = NODE_STATE(ksIdleThread);
}

void
setDomain(tcb_t *tptr, dom_t dom)
{
    tcbSchedDequeue(tptr);
    tptr->tcbDomain = dom;
    if (isSchedulable(tptr)) {
        SCHED_ENQUEUE(tptr);
    }
    if (tptr == NODE_STATE(ksCurThread)) {
        rescheduleRequired();
    }
}

void
setMCPriority(tcb_t *tptr, prio_t mcp)
{
    tptr->tcbMCP = mcp;
}

void
setPriority(tcb_t *tptr, prio_t prio)
{
    tcbSchedDequeue(tptr);
    tptr->tcbPriority = prio;

    switch (thread_state_get_tsType(tptr->tcbState)) {
    case ThreadState_Running:
    case ThreadState_Restart:
        if (isSchedulable(tptr)) {
            SCHED_ENQUEUE(tptr);
            if (tptr == NODE_STATE(ksCurThread)) {
                rescheduleRequired();
            }
        }
        break;
    case ThreadState_BlockedOnReceive:
    case ThreadState_BlockedOnSend:
        reorderEP(EP_PTR(thread_state_get_blockingObject(tptr->tcbState)), tptr);
        break;
    case ThreadState_BlockedOnNotification:
        reorderNTFN(NTFN_PTR(thread_state_get_blockingObject(tptr->tcbState)), tptr);
        break;
    default:
        break;
    }
}

static void
possibleSwitchTo(tcb_t* target, bool_t onSamePriority)
{
    if (target->tcbSchedContext != NULL && !thread_state_get_tcbInReleaseQueue(target->tcbState)) {
        dom_t curDom = ksCurDomain;
        prio_t curPrio = NODE_STATE(ksCurThread)->tcbPriority;
        dom_t targetDom = target->tcbDomain;
        prio_t targetPrio = target->tcbPriority;
        tcb_t *action = NODE_STATE(ksSchedulerAction);

        if (targetDom != curDom) {
            SCHED_ENQUEUE(target);
        } else {
            assert(isRunnable(target));
            assert(refill_sufficient(target->tcbSchedContext, 0));
            assert(refill_ready(target->tcbSchedContext));
            if ((targetPrio > curPrio || (targetPrio == curPrio && onSamePriority))
                    && action == SchedulerAction_ResumeCurrentThread &&
                    SMP_TERNARY(target->tcbSchedContext->scCore == getCurrentCPUIndex(), true)) {
                NODE_STATE(ksSchedulerAction) = target;
            } else {
                SCHED_ENQUEUE(target);
            }
            if (action != SchedulerAction_ResumeCurrentThread
                    && action != SchedulerAction_ChooseNewThread) {
                rescheduleRequired();
            }
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
    if (tptr == NODE_STATE(ksCurThread) &&
            NODE_STATE(ksSchedulerAction) == SchedulerAction_ResumeCurrentThread &&
            !isSchedulable(tptr)) {
        rescheduleRequired();
    }
}

void
postpone(sched_context_t *sc)
{
    tcbSchedDequeue(sc->scTcb);
    tcbReleaseEnqueue(sc->scTcb);
    NODE_STATE_ON_CORE(ksReprogram, sc->scCore) = true;
}

void
setNextInterrupt(void)
{
    time_t next_interrupt = NODE_STATE(ksCurTime) +
                            REFILL_HEAD(NODE_STATE(ksCurThread)->tcbSchedContext).rAmount;

    if (CONFIG_NUM_DOMAINS > 1) {
        next_interrupt = MIN(next_interrupt, NODE_STATE(ksCurTime) + ksDomainTime);
    }

    if (NODE_STATE(ksReleaseHead) != NULL) {
        next_interrupt = MIN(REFILL_HEAD(NODE_STATE(ksReleaseHead)->tcbSchedContext).rTime, next_interrupt);
    }

    setDeadline(next_interrupt - getTimerPrecision());
}

void
chargeBudget(ticks_t capacity)
{
    if (capacity == 0) {
        NODE_STATE(ksConsumed) = refill_budget_check(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed));
    }

    capacity = refill_capacity(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed));
    if (capacity > 0  && NODE_STATE(ksConsumed) > 0 && refill_ready(NODE_STATE(ksCurSC))) {
        refill_split_check(NODE_STATE(ksCurSC), NODE_STATE(ksConsumed));
    }

    NODE_STATE(ksConsumed) = 0;
    if (likely(isRunnable(NODE_STATE(ksCurThread)))) {
        endTimeslice();
        rescheduleRequired();
        NODE_STATE(ksReprogram) = true;
    }
}

void
endTimeslice(void)
{
    assert(isRunnable(NODE_STATE(ksCurSC->scTcb)));
    if (refill_ready(NODE_STATE(ksCurSC)) && refill_sufficient(NODE_STATE(ksCurSC), 0)) {
        /* apply round robin */
        assert(refill_sufficient(NODE_STATE(ksCurSC), 0));
        assert(!thread_state_get_tcbQueued(NODE_STATE(ksCurThread)->tcbState));
        SCHED_APPEND_CURRENT_TCB;
    } else {
        /* postpone until ready */
        postpone(NODE_STATE(ksCurSC));
    }
}

void
rescheduleRequired(void)
{
    if (NODE_STATE(ksSchedulerAction) != SchedulerAction_ResumeCurrentThread
            && NODE_STATE(ksSchedulerAction) != SchedulerAction_ChooseNewThread &&
            isSchedulable(NODE_STATE(ksSchedulerAction))) {
        assert(refill_sufficient(NODE_STATE(ksSchedulerAction)->tcbSchedContext, 0));
        assert(refill_ready(NODE_STATE(ksSchedulerAction)->tcbSchedContext));
        SCHED_ENQUEUE(NODE_STATE(ksSchedulerAction));
    }
    NODE_STATE(ksSchedulerAction) = SchedulerAction_ChooseNewThread;
}

void
awaken(void)
{
    while (unlikely(NODE_STATE(ksReleaseHead) != NULL && refill_ready(NODE_STATE(ksReleaseHead)->tcbSchedContext))) {
        tcb_t *awakened = tcbReleaseDequeue();
        SMP_COND_STATEMENT(assert(awakened->tcbAffinity == getCurrentCPUIndex()));
        refill_unblock_check(awakened->tcbSchedContext);
        if (unlikely(!refill_ready(awakened->tcbSchedContext))) {
            tcbReleaseEnqueue(awakened);
        } else {
            assert(refill_sufficient(awakened->tcbSchedContext, 0));
            tcbSchedAppend(awakened);
            switchIfRequiredTo(awakened);
        }
    }
}
