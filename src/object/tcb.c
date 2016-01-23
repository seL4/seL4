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
#include <api/failures.h>
#include <api/invocation.h>
#include <api/syscall.h>
#include <machine/io.h>
#include <object/structures.h>
#include <object/objecttype.h>
#include <object/cnode.h>
#include <object/tcb.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <model/statedata.h>
#include <util.h>
#include <string.h>

#define NULL_PRIO (seL4_Prio_new(seL4_MinPrio, seL4_MinPrio))

static inline bool_t
validFaultEndpoint(cap_t cap)
{
    if (cap_get_capType(cap) == cap_endpoint_cap) {
        return cap_endpoint_cap_get_capCanSend(cap) &&
               cap_endpoint_cap_get_capCanGrant(cap);
    }

    return cap_get_capType(cap) == cap_null_cap;
}

static exception_t
checkMCP(prio_t mcp)
{
    /* can't create a thread with mcp greater than our own */
    if (mcp > ksCurThread->tcbMCP) {
        userError("TCB Configure: Requested  maximum controlled priority %lu too high (max %lu).",
                  (unsigned long) mcp, (unsigned long) ksCurThread->tcbMCP);
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return EXCEPTION_NONE;
}

static exception_t
checkPrio(prio_t prio)
{
    /* can't create a thread with prio greater than our own mcp */
    if (prio > ksCurThread->tcbMCP) {
        userError("TCB Configure: Requested priority %lu too high (max %lu).",
                  (unsigned long) prio, (unsigned long) ksCurThread->tcbMCP);
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return EXCEPTION_NONE;
}

static inline void
addToBitmap(word_t prio)
{
    word_t l1index;

    l1index = prio_to_l1index(prio);
    ksReadyQueuesL1Bitmap |= BIT(l1index);
    ksReadyQueuesL2Bitmap[l1index] |= BIT(prio & MASK(wordRadix));
}

static inline void
removeFromBitmap(word_t prio)
{
    word_t l1index;

    l1index = prio_to_l1index(prio);
    ksReadyQueuesL2Bitmap[l1index] &= ~BIT(prio & MASK(wordRadix));
    if (unlikely(!ksReadyQueuesL2Bitmap[l1index])) {
        ksReadyQueuesL1Bitmap &= ~BIT(l1index);
    }
}

/* Add TCB to the head of a scheduler queue */
void
tcbSchedEnqueue(tcb_t *tcb)
{
    assert(isSchedulable(tcb));
    assert(tcb->tcbSchedContext->scRemaining >= getKernelWcetTicks());

    if (!thread_state_get_tcbQueued(tcb->tcbState)) {
        tcb_queue_t queue;
        prio_t prio;
        word_t idx;

        prio = tcb->tcbPriority;
        idx = prio;
        queue = ksReadyQueues[idx];

        if (!queue.end) { /* Empty list */
            queue.end = tcb;
            addToBitmap(prio);
        } else {
            queue.head->tcbSchedPrev = tcb;
        }
        tcb->tcbSchedPrev = NULL;
        tcb->tcbSchedNext = queue.head;
        queue.head = tcb;

        ksReadyQueues[idx] = queue;

        thread_state_ptr_set_tcbQueued(&tcb->tcbState, true);
    }
}

/* Add TCB to the end of a scheduler queue */
void
tcbSchedAppend(tcb_t *tcb)
{
    assert(isSchedulable(tcb));
    assert(tcb->tcbSchedContext->scRemaining >= getKernelWcetTicks());

    if (!thread_state_get_tcbQueued(tcb->tcbState)) {
        tcb_queue_t queue;
        prio_t prio;
        word_t idx;

        prio = tcb->tcbPriority;
        idx = prio;
        queue = ksReadyQueues[idx];

        if (!queue.head) { /* Empty list */
            queue.head = tcb;
            addToBitmap(prio);
        } else {
            queue.end->tcbSchedNext = tcb;
        }
        tcb->tcbSchedPrev = queue.end;
        tcb->tcbSchedNext = NULL;
        queue.end = tcb;

        ksReadyQueues[idx] = queue;

        thread_state_ptr_set_tcbQueued(&tcb->tcbState, true);
    }
}

/* Remove TCB from a scheduler queue */
void
tcbSchedDequeue(tcb_t *tcb)
{
    if (thread_state_get_tcbQueued(tcb->tcbState)) {
        tcb_queue_t queue;
        prio_t prio;
        word_t idx;

        prio = tcb->tcbPriority;
        idx = prio;
        queue = ksReadyQueues[idx];

        if (tcb->tcbSchedPrev) {
            tcb->tcbSchedPrev->tcbSchedNext = tcb->tcbSchedNext;
        } else {
            queue.head = tcb->tcbSchedNext;
            if (likely(!tcb->tcbSchedNext)) {
                removeFromBitmap(prio);
            }
        }

        if (tcb->tcbSchedNext) {
            tcb->tcbSchedNext->tcbSchedPrev = tcb->tcbSchedPrev;
        } else {
            queue.end = tcb->tcbSchedPrev;
        }

        ksReadyQueues[idx] = queue;

        thread_state_ptr_set_tcbQueued(&tcb->tcbState, false);
    }
}

/* remove a TCB from the release queue */
void
tcbReleaseRemove(tcb_t *tcb)
{
    if (likely(thread_state_get_tcbInReleaseQueue(tcb->tcbState))) {
        if (tcb->tcbSchedPrev) {
            tcb->tcbSchedPrev->tcbSchedNext = tcb->tcbSchedNext;
            tcb->tcbSchedPrev = NULL;
        } else {
            ksReleaseHead = tcb->tcbSchedNext;
            /* the head has changed, we might need to set an new timeout */
            ksReprogram = true;
        }

        if (tcb->tcbSchedNext) {
            tcb->tcbSchedNext->tcbSchedPrev = tcb->tcbSchedPrev;
            tcb->tcbSchedNext = NULL;
        }

        thread_state_ptr_set_tcbInReleaseQueue(&tcb->tcbState, false);
    }
}

/* insert a TCB into the release queue - ordered */
void
tcbReleaseEnqueue(tcb_t *tcb)
{
    assert(thread_state_get_tcbInReleaseQueue(tcb->tcbState) == false);

    if (ksReleaseHead == NULL ||
            tcb->tcbSchedContext->scNext < ksReleaseHead->tcbSchedContext->scNext) {
        /* insert at head */
        tcb->tcbSchedNext = ksReleaseHead;
        tcb->tcbSchedPrev = NULL;
        ksReleaseHead = tcb;
        if (tcb->tcbSchedNext) {
            tcb->tcbSchedNext->tcbSchedPrev = tcb;
        }
        ksReprogram = true;
    } else {
        /* find a place in the list */
        tcb_t *node = ksReleaseHead;
        tcb_t *prev = NULL;
        while (node != NULL && tcb->tcbSchedContext->scNext >= node->tcbSchedContext->scNext) {
            prev = node;
            node = node->tcbSchedNext;
        }

        /* prev cannot be NULL or we would have taken the other branch */
        assert(prev != NULL);

        tcb->tcbSchedNext = node;
        tcb->tcbSchedPrev = prev;
        prev->tcbSchedNext = tcb;

        if (node != NULL) {
            node->tcbSchedPrev = tcb;
        }
    }

    thread_state_ptr_set_tcbInReleaseQueue(&tcb->tcbState, true);
}

/* remove the head of the release queue */
tcb_t *
tcbReleaseDequeue(void)
{
    tcb_t *detached_head;

    assert(ksReleaseHead != NULL);
    assert(ksReleaseHead->tcbSchedPrev == NULL);

    detached_head = ksReleaseHead;
    ksReleaseHead = ksReleaseHead->tcbSchedNext;

    if (detached_head->tcbSchedNext) {
        detached_head->tcbSchedNext->tcbSchedPrev = NULL;
        detached_head->tcbSchedNext = NULL;
    }

    thread_state_ptr_set_inReleaseQueue(&detached_head->tcbState, false);
    ksReprogram = true;

    return detached_head;
}

/* Add TCB to the ordered endpoint queue */
tcb_queue_t
tcbEPAppend(tcb_t *tcb, tcb_queue_t queue)
{
    if (!queue.head) { /* Empty list */
        queue.head = tcb;
        queue.end = tcb;
        tcb->tcbEPPrev = NULL;
        tcb->tcbEPNext = NULL;
    } else {
        /* insert ordered */
        tcb_t *prev = NULL;
        tcb_t *current = queue.head;

        /* find a place to put the tcb */
        while (current != NULL && tcb->tcbPriority <= current->tcbPriority) {
            prev = current;
            current = current->tcbEPNext;
        }

        /* there is at least one other tcb in the queue (since queue.head exists)
         * so we are inserting distinctly at the head, tail or middle. */
        if (prev == NULL) {
            /* insert at head */
            queue.head = tcb;
            tcb->tcbEPNext = current;
            tcb->tcbEPPrev = NULL;
            current->tcbEPPrev = tcb;
        } else if (current == NULL) {
            /* insert at end */
            queue.end = tcb;
            prev->tcbEPNext = tcb;
            tcb->tcbEPPrev = prev;
            tcb->tcbEPNext = NULL;
        } else {
            /* insert between current and prev */
            prev->tcbEPNext = tcb;
            current->tcbEPPrev = tcb;
            tcb->tcbEPPrev = prev;
            tcb->tcbEPNext = current;
        }
    }

    return queue;
}

/* Remove TCB from an endpoint queue */
tcb_queue_t
tcbEPDequeue(tcb_t *tcb, tcb_queue_t queue)
{
    if (tcb->tcbEPPrev) {
        tcb->tcbEPPrev->tcbEPNext = tcb->tcbEPNext;
    } else {
        queue.head = tcb->tcbEPNext;
    }

    if (tcb->tcbEPNext) {
        tcb->tcbEPNext->tcbEPPrev = tcb->tcbEPPrev;
    } else {
        queue.end = tcb->tcbEPPrev;
    }

    return queue;
}

/* reorder a tcb in an ipc endpoint queue */
tcb_queue_t
tcbEPReorder(tcb_t *tcb, tcb_queue_t queue, prio_t oldPrio)
{
    word_t newPrio = tcb->tcbPriority;

    if (newPrio == oldPrio) {
        /* nothing to do, prio didn't change */
    } else if (newPrio > oldPrio) {
        /* move tcb up in the queue */
        tcb_t *prev = tcb->tcbEPPrev;

        if (prev == NULL || newPrio <= prev->tcbPriority) {
            /* nothing to do, tcb is at head of the list or in the right place */
            return queue;
        }

        /* tcb is not in the right place - take it out */
        queue = tcbEPDequeue(tcb, queue);

        /* now find the correct place */
        while (prev != NULL && prev->tcbPriority < newPrio) {
            prev = prev->tcbEPPrev;
        }

        /* this can't happen, as if we are being placed at the end of the queue we have
         * the lowest prio in the queue and aren't changing position -> which means we exited above */
        assert(prev != queue.end);

        if (prev == NULL) {
            /* place at head */
            tcb->tcbEPNext = queue.head;
            tcb->tcbEPPrev = NULL;
            queue.head->tcbEPPrev = tcb;
            queue.head = tcb;
        } else {
            /* tcb goes after prev */
            tcb->tcbEPNext = prev->tcbEPNext;
            tcb->tcbEPPrev = prev;
            /* tcb->tcbEPPrev can't be NULL as we would have taken the first branch */
            assert(tcb->tcbEPPrev != NULL);
            tcb->tcbEPNext->tcbEPPrev = tcb;
            prev->tcbEPNext = tcb;
        }
    } else { /* newPrio < oldPrio */
        /* move tcb down in the queue */
        tcb_t *next = tcb->tcbEPNext;

        if (next == NULL || newPrio >= next->tcbPriority) {
            /* nothing to do, tcb is at the tail of the list or in the correct place */
            return queue;
        }

        /* tcb is not in the right place - take it out */
        queue = tcbEPDequeue(tcb, queue);

        /* now find the correct place */
        while (next != NULL && next->tcbPriority >= newPrio) {
            next = next->tcbEPNext;
        }

        /* this can't happen, as if we are being placed at the head of the queue we have
         * the highest prio in the queue and aren't changing potition -> which means
         * we exited above */
        assert(next != queue.head);

        if (next == NULL) {
            /* tcb goes to the tail */
            tcb->tcbEPPrev = queue.end;
            tcb->tcbEPNext = NULL;
            queue.end->tcbEPNext = tcb;
            queue.end = tcb;
        } else {
            /* tcb goes before next */
            tcb->tcbEPNext = next;
            tcb->tcbEPPrev = next->tcbEPPrev;
            next->tcbEPPrev->tcbEPNext = tcb;
            next->tcbEPPrev = tcb;
        }
    }

    return queue;
}


cptr_t PURE
getExtraCPtr(word_t *bufferPtr, word_t i)
{
    return (cptr_t)bufferPtr[seL4_MsgMaxLength + 2 + i];
}

void
setExtraBadge(word_t *bufferPtr, word_t badge,
              word_t i)
{
    bufferPtr[seL4_MsgMaxLength + 2 + i] = badge;
}

void
setupCallerCap(tcb_t *sender, tcb_t *receiver, sched_context_t *donated)
{
    cte_t *replySlot, *callerSlot;
    cap_t masterCap UNUSED, callerCap UNUSED;

    setThreadState(sender, ThreadState_BlockedOnReply);
    replySlot = TCB_PTR_CTE_PTR(sender, tcbReply);
    masterCap = replySlot->cap;
    /* Haskell error: "Sender must have a valid master reply cap" */
    assert(cap_get_capType(masterCap) == cap_reply_cap);
    assert(cap_reply_cap_get_capReplyMaster(masterCap));
    assert(TCB_PTR(cap_reply_cap_get_capTCBPtr(masterCap)) == sender);
    callerSlot = TCB_PTR_CTE_PTR(receiver, tcbCaller);
    callerCap = callerSlot->cap;
    /* Haskell error: "Caller cap must not already exist" */
    assert(cap_get_capType(callerCap) == cap_null_cap);
    cteInsert(cap_reply_cap_new(false, TCB_REF(sender), SC_REF(donated)),
              replySlot, callerSlot);
    if (donated != NULL) {
        donated->scReply = sender;
    }
}

void
deleteCallerCap(tcb_t *receiver)
{
    cte_t *callerSlot;

    callerSlot = TCB_PTR_CTE_PTR(receiver, tcbCaller);
    /** GHOSTUPD: "(True, gs_set_assn cteDeleteOne_'proc (ucast cap_reply_cap))" */
    cteDeleteOne(callerSlot);
}

extra_caps_t current_extra_caps;

exception_t
lookupExtraCaps(tcb_t* thread, word_t *bufferPtr, seL4_MessageInfo_t info)
{
    lookupSlot_raw_ret_t lu_ret;
    cptr_t cptr;
    word_t i, length;

    if (!bufferPtr) {
        current_extra_caps.excaprefs[0] = NULL;
        return EXCEPTION_NONE;
    }

    length = seL4_MessageInfo_get_extraCaps(info);

    for (i = 0; i < length; i++) {
        cptr = getExtraCPtr(bufferPtr, i);

        lu_ret = lookupSlot(thread, cptr);
        if (lu_ret.status != EXCEPTION_NONE) {
            current_fault = fault_cap_fault_new(cptr, false);
            return lu_ret.status;
        }

        current_extra_caps.excaprefs[i] = lu_ret.slot;
    }
    if (i < seL4_MsgMaxExtraCaps) {
        current_extra_caps.excaprefs[i] = NULL;
    }

    return EXCEPTION_NONE;
}

/* Copy IPC MRs from one thread to another */
word_t
copyMRs(tcb_t *sender, word_t *sendBuf, tcb_t *receiver,
        word_t *recvBuf, word_t n)
{
    word_t i;

    /* Copy inline words */
    for (i = 0; i < n && i < n_msgRegisters; i++) {
        setRegister(receiver, msgRegisters[i],
                    getRegister(sender, msgRegisters[i]));
    }

    if (!recvBuf || !sendBuf) {
        return i;
    }

    /* Copy out-of-line words */
    for (; i < n; i++) {
        recvBuf[i + 1] = sendBuf[i + 1];
    }

    return i;
}

/* The following functions sit in the syscall error monad, but include the
 * exception cases for the preemptible bottom end, as they call the invoke
 * functions directly.  This is a significant deviation from the Haskell
 * spec. */
exception_t
decodeTCBInvocation(word_t invLabel, word_t length, cap_t cap,
                    cte_t* slot, extra_caps_t excaps, bool_t call,
                    word_t *buffer)
{
    switch (invLabel) {
    case TCBReadRegisters:
        /* Second level of decoding */
        return decodeReadRegisters(cap, length, call, buffer);

    case TCBWriteRegisters:
        return decodeWriteRegisters(cap, length, buffer);

    case TCBCopyRegisters:
        return decodeCopyRegisters(cap, length, excaps, buffer);

    case TCBSuspend:
        /* Jump straight to the invoke */
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeTCB_Suspend(
                   TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)));

    case TCBResume:
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeTCB_Resume(
                   TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)));

    case TCBConfigure:
        return decodeTCBConfigure(cap, length, slot, excaps, buffer);

    case TCBSetPriority:
        return decodeSetPriority(cap, length, buffer);

    case TCBSetMCPriority:
        return decodeSetMCPriority(cap, length, buffer);

    case TCBSetIPCBuffer:
        return decodeSetIPCBuffer(cap, length, slot, excaps, buffer);

    case TCBSetSpace:
        return decodeSetSpace(cap, length, slot, excaps, buffer);

    case TCBBindNotification:
        return decodeBindNotification(cap, excaps);

    case TCBUnbindNotification:
        return decodeUnbindNotification(cap);

    default:
        /* Haskell: "throw IllegalOperation" */
        userError("TCB: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

enum CopyRegistersFlags {
    CopyRegisters_suspendSource = 0,
    CopyRegisters_resumeTarget = 1,
    CopyRegisters_transferFrame = 2,
    CopyRegisters_transferInteger = 3
};

exception_t
decodeCopyRegisters(cap_t cap, word_t length,
                    extra_caps_t excaps, word_t *buffer)
{
    word_t transferArch;
    tcb_t *srcTCB;
    cap_t source_cap;
    word_t flags;

    if (length < 1 || excaps.excaprefs[0] == NULL) {
        userError("TCB CopyRegisters: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    flags = getSyscallArg(0, buffer);

    transferArch = Arch_decodeTransfer(flags >> 8);

    source_cap = excaps.excaprefs[0]->cap;

    if (cap_get_capType(source_cap) == cap_thread_cap) {
        srcTCB = TCB_PTR(cap_thread_cap_get_capTCBPtr(source_cap));
    } else {
        userError("TCB CopyRegisters: Invalid source TCB.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_CopyRegisters(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), srcTCB,
               flags & BIT(CopyRegisters_suspendSource),
               flags & BIT(CopyRegisters_resumeTarget),
               flags & BIT(CopyRegisters_transferFrame),
               flags & BIT(CopyRegisters_transferInteger),
               transferArch);

}

enum ReadRegistersFlags {
    ReadRegisters_suspend = 0
};

exception_t
decodeReadRegisters(cap_t cap, word_t length, bool_t call,
                    word_t *buffer)
{
    word_t transferArch, flags, n;
    tcb_t* thread;

    if (length < 2) {
        userError("TCB ReadRegisters: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    flags = getSyscallArg(0, buffer);
    n     = getSyscallArg(1, buffer);

    if (n < 1 || n > n_frameRegisters + n_gpRegisters) {
        userError("TCB ReadRegisters: Attempted to read an invalid number of registers (%d).",
                  (int)n);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 1;
        current_syscall_error.rangeErrorMax = n_frameRegisters +
                                              n_gpRegisters;
        return EXCEPTION_SYSCALL_ERROR;
    }

    transferArch = Arch_decodeTransfer(flags >> 8);

    thread = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    if (thread == ksCurThread) {
        userError("TCB ReadRegisters: Attempted to read our own registers.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ReadRegisters(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)),
               flags & BIT(ReadRegisters_suspend),
               n, transferArch, call);
}

enum WriteRegistersFlags {
    WriteRegisters_resume = 0
};

exception_t
decodeWriteRegisters(cap_t cap, word_t length, word_t *buffer)
{
    word_t flags, w;
    word_t transferArch;
    tcb_t* thread;

    if (length < 2) {
        userError("TCB WriteRegisters: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    flags = getSyscallArg(0, buffer);
    w     = getSyscallArg(1, buffer);

    if (length - 2 < w) {
        userError("TCB WriteRegisters: Message too short for requested write size (%d/%d).",
                  (int)(length - 2), (int)w);
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    transferArch = Arch_decodeTransfer(flags >> 8);

    thread = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    if (thread == ksCurThread) {
        userError("TCB WriteRegisters: Attempted to write our own registers.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_WriteRegisters(thread,
                                    flags & BIT(WriteRegisters_resume),
                                    w, transferArch, buffer);
}

/* SetPriority, SetIPCParams and SetSpace are all
 * specialisations of TCBConfigure. */

exception_t
decodeTCBConfigure(cap_t cap, word_t length, cte_t* slot,
                   extra_caps_t rootCaps, word_t *buffer)
{
    cte_t *bufferSlot, *cRootSlot, *vRootSlot, *scSlot, *fepSlot, *tfepSlot;
    cap_t bufferCap, cRootCap, vRootCap, scCap, fepCap, tfepCap;
    sched_context_t *sched_context;
    deriveCap_ret_t dc_ret;
    word_t cRootData, vRootData, bufferAddr;
    seL4_Prio_t prio;
    exception_t status;

    if (length < 4 || rootCaps.excaprefs[0] == NULL
            || rootCaps.excaprefs[1] == NULL
            || rootCaps.excaprefs[2] == NULL
            || rootCaps.excaprefs[3] == NULL
            || rootCaps.excaprefs[4] == NULL
            || rootCaps.excaprefs[5] == NULL) {
        userError("TCB Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    prio.words[0] = getSyscallArg(0, buffer);
    cRootData     = getSyscallArg(1, buffer);
    vRootData     = getSyscallArg(2, buffer);
    bufferAddr    = getSyscallArg(3, buffer);

    fepSlot    = rootCaps.excaprefs[0];
    fepCap     = rootCaps.excaprefs[0]->cap;
    tfepSlot   = rootCaps.excaprefs[1];
    tfepCap    = rootCaps.excaprefs[1]->cap;
    scSlot     = rootCaps.excaprefs[2];
    scCap      = rootCaps.excaprefs[2]->cap;
    cRootSlot  = rootCaps.excaprefs[3];
    cRootCap   = rootCaps.excaprefs[3]->cap;
    vRootSlot  = rootCaps.excaprefs[4];
    vRootCap   = rootCaps.excaprefs[4]->cap;
    bufferSlot = rootCaps.excaprefs[5];
    bufferCap  = rootCaps.excaprefs[5]->cap;

    status = checkPrio(seL4_Prio_get_prio(prio));
    if (status != EXCEPTION_NONE) {
        return status;
    }

    status = checkMCP(seL4_Prio_get_mcp(prio));
    if (status != EXCEPTION_NONE) {
        return status;
    }

    if (bufferAddr == 0) {
        bufferSlot = NULL;
    } else {
        exception_t e;

        dc_ret = deriveCap(bufferSlot, bufferCap);
        if (dc_ret.status != EXCEPTION_NONE) {
            return dc_ret.status;
        }
        bufferCap = dc_ret.cap;
        e = checkValidIPCBuffer(bufferAddr, bufferCap);
        if (e != EXCEPTION_NONE) {
            return e;
        }
    }

    if (slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbCTable)) ||
            slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbVTable))) {
        userError("TCB Configure: CSpace or VSpace currently being deleted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cRootData != 0) {
        cRootCap = updateCapData(false, cRootData, cRootCap);
    }

    dc_ret = deriveCap(cRootSlot, cRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    cRootCap = dc_ret.cap;

    if (cap_get_capType(cRootCap) != cap_cnode_cap) {
        userError("TCB Configure: CSpace cap is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (vRootData != 0) {
        vRootCap = updateCapData(false, vRootData, vRootCap);
    }

    dc_ret = deriveCap(vRootSlot, vRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    vRootCap = dc_ret.cap;

    if (!isValidVTableRoot(vRootCap)) {
        userError("TCB Configure: VSpace cap is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    dc_ret = deriveCap(scSlot, scCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    scCap = dc_ret.cap;

    switch (cap_get_capType(scCap)) {
    case cap_sched_context_cap:
        sched_context = SC_PTR(cap_sched_context_cap_get_capPtr(scCap));
        break;
    case cap_null_cap:
        sched_context = NULL;
        break;
    default:
        userError("TCB Configure: sched context cap is invalid");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 2;
        return EXCEPTION_SYSCALL_ERROR;
    }

    dc_ret = deriveCap(fepSlot, fepCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        userError("TCB Configure: fault ep invalid");
        return dc_ret.status;
    }
    fepCap = dc_ret.cap;

    if (!validFaultEndpoint(fepCap)) {
        userError("TCB configure: fault endpoint cap must be either null cap or endpoint cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    dc_ret = deriveCap(tfepSlot, tfepCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        userError("TCB Configure: temporal fault ep invalid");
        return dc_ret.status;
    }
    tfepCap = dc_ret.cap;

    if (!validFaultEndpoint(tfepCap)) {
        userError("TCB configure: temporal fault endpoint cap must be either null cap or endpoint cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 2;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), slot,
               fepCap, fepSlot,
               tfepCap, tfepSlot,
               prio,
               cRootCap, cRootSlot,
               vRootCap, vRootSlot,
               bufferAddr, bufferCap,
               bufferSlot, sched_context, thread_control_update_all);
}

exception_t
decodeSetPriority(cap_t cap, word_t length, word_t *buffer)
{
    prio_t newPrio;
    exception_t status;
    tcb_t *tcb;

    if (length < 1) {
        userError("TCB SetPriority: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    newPrio = (prio_t) getSyscallArg(0, buffer);

    status = checkPrio(newPrio);
    if (status != EXCEPTION_NONE) {
        return status;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               tcb, NULL, cap_null_cap_new(), 0, cap_null_cap_new(),
               0, seL4_Prio_new(newPrio, tcb->tcbMCP),
               cap_null_cap_new(), NULL,
               cap_null_cap_new(), NULL,
               0, cap_null_cap_new(), NULL,
               NULL, thread_control_update_priority);
}

exception_t
decodeSetMCPriority(cap_t cap, word_t length, word_t *buffer)
{
    prio_t newMcp;
    exception_t status;
    tcb_t *tcb;

    if (length < 1) {
        userError("TCB SetPriority: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    newMcp = (prio_t) getSyscallArg(0, buffer);

    status = checkMCP(newMcp);
    if (status != EXCEPTION_NONE) {
        return status;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               tcb, NULL, cap_null_cap_new(),
               0, cap_null_cap_new(),
               0, seL4_Prio_new(tcb->tcbPriority, newMcp),
               cap_null_cap_new(), NULL,
               cap_null_cap_new(), NULL,
               0, cap_null_cap_new(), NULL,
               NULL, thread_control_update_priority);
}

exception_t
decodeSetIPCBuffer(cap_t cap, word_t length, cte_t* slot,
                   extra_caps_t excaps, word_t *buffer)
{
    cptr_t cptr_bufferPtr;
    cap_t bufferCap;
    cte_t *bufferSlot;

    if (length < 1 || excaps.excaprefs[0] == NULL) {
        userError("TCB SetIPCBuffer: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cptr_bufferPtr  = getSyscallArg(0, buffer);
    bufferSlot = excaps.excaprefs[0];
    bufferCap  = excaps.excaprefs[0]->cap;

    if (cptr_bufferPtr == 0) {
        bufferSlot = NULL;
    } else {
        exception_t e;
        deriveCap_ret_t dc_ret;

        dc_ret = deriveCap(bufferSlot, bufferCap);
        if (dc_ret.status != EXCEPTION_NONE) {
            return dc_ret.status;
        }
        bufferCap = dc_ret.cap;
        e = checkValidIPCBuffer(cptr_bufferPtr, bufferCap);
        if (e != EXCEPTION_NONE) {
            return e;
        }
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), slot,
               cap_null_cap_new(),
               0, cap_null_cap_new(), 0,
               NULL_PRIO,
               cap_null_cap_new(), NULL,
               cap_null_cap_new(), NULL,
               cptr_bufferPtr, bufferCap,
               bufferSlot, NULL, thread_control_update_ipc_buffer);
}

exception_t
decodeSetSpace(cap_t cap, word_t length, cte_t* slot,
               extra_caps_t excaps, word_t *buffer)
{
    word_t cRootData, vRootData;
    cte_t *cRootSlot, *vRootSlot, *fepSlot, *tfepSlot;
    cap_t cRootCap, vRootCap, fepCap, tfepCap;
    deriveCap_ret_t dc_ret;

    if (length < 2 || excaps.excaprefs[0] == NULL
            || excaps.excaprefs[1] == NULL
            || excaps.excaprefs[2] == NULL
            || excaps.excaprefs[3] == NULL) {
        userError("TCB SetSpace: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cRootData = getSyscallArg(0, buffer);
    vRootData = getSyscallArg(1, buffer);

    fepSlot    = excaps.excaprefs[0];
    fepCap     = excaps.excaprefs[0]->cap;
    tfepSlot   = excaps.excaprefs[1];
    tfepCap    = excaps.excaprefs[1]->cap;
    cRootSlot  = excaps.excaprefs[2];
    cRootCap   = excaps.excaprefs[2]->cap;
    vRootSlot  = excaps.excaprefs[3];
    vRootCap   = excaps.excaprefs[3]->cap;

    if (slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbCTable)) ||
            slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbVTable))) {
        userError("TCB SetSpace: CSpace or VSpace currently being deleted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cRootData != 0) {
        cRootCap = updateCapData(false, cRootData, cRootCap);
    }

    dc_ret = deriveCap(cRootSlot, cRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    cRootCap = dc_ret.cap;

    if (cap_get_capType(cRootCap) != cap_cnode_cap) {
        userError("TCB SetSpace: Invalid CNode cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (vRootData != 0) {
        vRootCap = updateCapData(false, vRootData, vRootCap);
    }

    dc_ret = deriveCap(vRootSlot, vRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    vRootCap = dc_ret.cap;

    if (!isValidVTableRoot(vRootCap)) {
        userError("TCB SetSpace: Invalid VSpace cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    dc_ret = deriveCap(fepSlot, fepCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }

    fepCap = dc_ret.cap;;

    if (!validFaultEndpoint(fepCap)) {
        userError("TCB SetSpace: fault endpoint cap must be either null cap or endpoint cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_NONE;
    }

    dc_ret = deriveCap(tfepSlot, tfepCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }

    tfepCap = dc_ret.cap;;

    if (!validFaultEndpoint(tfepCap)) {
        userError("TCB SetSpace: fault endpoint cap must be either null cap or endpoint cap");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 2;
        return EXCEPTION_NONE;
    }


    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), slot,
               fepCap, fepSlot,
               tfepCap, tfepSlot,
               NULL_PRIO,
               cRootCap, cRootSlot,
               vRootCap, vRootSlot,
               0, cap_null_cap_new(), NULL, NULL, thread_control_update_space);
}

exception_t
decodeBindNotification(cap_t cap, extra_caps_t excaps)
{
    notification_t *ntfnPtr;
    tcb_t *tcb;
    cap_t ntfn_cap;

    if (excaps.excaprefs[0] == NULL) {
        userError("TCB BindNotification: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));

    if (tcb->tcbBoundNotification) {
        userError("TCB BindNotification: TCB already has a bound notification.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    ntfn_cap = excaps.excaprefs[0]->cap;

    if (cap_get_capType(ntfn_cap) == cap_notification_cap) {
        ntfnPtr = NTFN_PTR(cap_notification_cap_get_capNtfnPtr(ntfn_cap));
    } else {
        userError("TCB BindNotification: Notification is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (!cap_notification_cap_get_capNtfnCanReceive(ntfn_cap)) {
        userError("TCB BindNotification: Insufficient access rights");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if ((tcb_t*)notification_ptr_get_ntfnQueue_head(ntfnPtr)
            || (tcb_t*)notification_ptr_get_ntfnBoundTCB(ntfnPtr)) {
        userError("TCB BindNotification: Notification cannot be bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }


    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_NotificationControl(tcb, ntfnPtr);
}

exception_t
decodeUnbindNotification(cap_t cap)
{
    tcb_t *tcb;

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));

    if (!tcb->tcbBoundNotification) {
        userError("TCB UnbindNotification: TCB already has no bound Notification.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_NotificationControl(tcb, NULL);
}

/* The following functions sit in the preemption monad and implement the
 * preemptible, non-faulting bottom end of a TCB invocation. */
exception_t
invokeTCB_Suspend(tcb_t *thread)
{
    suspend(thread);
    return EXCEPTION_NONE;
}

exception_t
invokeTCB_Resume(tcb_t *thread)
{
    restart(thread);
    return EXCEPTION_NONE;
}

exception_t
invokeTCB_ThreadControl(tcb_t *target, cte_t* slot,
                        cap_t fepCap, cte_t *fepSlot,
                        cap_t tfepCap, cte_t *tfepSlot,
                        seL4_Prio_t priority,
                        cap_t cRoot_newCap, cte_t *cRoot_srcSlot,
                        cap_t vRoot_newCap, cte_t *vRoot_srcSlot,
                        word_t bufferAddr, cap_t bufferCap,
                        cte_t *bufferSrcSlot,
                        sched_context_t *sched_context,
                        thread_control_flag_t updateFlags)
{
    exception_t e;
    cap_t tCap = cap_thread_cap_new((word_t)target);

    if (updateFlags & thread_control_update_space) {
        /* fault endpoint */
        cte_t *destSlot = TCB_PTR_CTE_PTR(target, tcbFaultHandler);
        cteDeleteOne(destSlot);
        if (cap_get_capType(fepCap) == cap_endpoint_cap) {
            cteInsert(fepCap, fepSlot, destSlot);
        }

        /* temporal fault endpoint */
        destSlot = TCB_PTR_CTE_PTR(target, tcbTemporalFaultHandler);
        cteDeleteOne(destSlot);
        if (cap_get_capType(tfepCap) == cap_endpoint_cap) {
            cteInsert(tfepCap, tfepSlot, destSlot);
        }
    }

    if (updateFlags & thread_control_update_priority) {
        setPriority(target, priority);
    }

    if (updateFlags & thread_control_update_sc) {
        if (sched_context != NULL) {
            schedContext_bindTCB(sched_context, target);
        } else {
            schedContext_unbindTCB(target->tcbSchedContext);
        }
    }

    if (updateFlags & thread_control_update_space) {
        cte_t *rootSlot;

        rootSlot = TCB_PTR_CTE_PTR(target, tcbCTable);
        e = cteDelete(rootSlot, true);
        if (e != EXCEPTION_NONE) {
            return e;
        }
        if (sameObjectAs(cRoot_newCap, cRoot_srcSlot->cap) &&
                sameObjectAs(tCap, slot->cap)) {
            cteInsert(cRoot_newCap, cRoot_srcSlot, rootSlot);
        }
    }

    if (updateFlags & thread_control_update_space) {
        cte_t *rootSlot;

        rootSlot = TCB_PTR_CTE_PTR(target, tcbVTable);
        e = cteDelete(rootSlot, true);
        if (e != EXCEPTION_NONE) {
            return e;
        }
        if (sameObjectAs(vRoot_newCap, vRoot_srcSlot->cap) &&
                sameObjectAs(tCap, slot->cap)) {
            cteInsert(vRoot_newCap, vRoot_srcSlot, rootSlot);
        }
    }

    if (updateFlags & thread_control_update_ipc_buffer) {
        cte_t *bufferSlot;

        bufferSlot = TCB_PTR_CTE_PTR(target, tcbBuffer);
        e = cteDelete(bufferSlot, true);
        if (e != EXCEPTION_NONE) {
            return e;
        }
        target->tcbIPCBuffer = bufferAddr;
        if (bufferSrcSlot && sameObjectAs(bufferCap, bufferSrcSlot->cap) &&
                sameObjectAs(tCap, slot->cap)) {
            cteInsert(bufferCap, bufferSrcSlot, bufferSlot);
        }
    }

    return EXCEPTION_NONE;
}

exception_t
invokeTCB_CopyRegisters(tcb_t *dest, tcb_t *tcb_src,
                        bool_t suspendSource, bool_t resumeTarget,
                        bool_t transferFrame, bool_t transferInteger,
                        word_t transferArch)
{
    if (suspendSource) {
        suspend(tcb_src);
    }

    if (resumeTarget) {
        restart(dest);
    }

    if (transferFrame) {
        word_t i;
        word_t v;
        word_t pc;

        for (i = 0; i < n_frameRegisters; i++) {
            v = getRegister(tcb_src, frameRegisters[i]);
            setRegister(dest, frameRegisters[i], v);
        }

        pc = getRestartPC(dest);
        setNextPC(dest, pc);
    }

    if (transferInteger) {
        word_t i;
        word_t v;

        for (i = 0; i < n_gpRegisters; i++) {
            v = getRegister(tcb_src, gpRegisters[i]);
            setRegister(dest, gpRegisters[i], v);
        }
    }

    return Arch_performTransfer(transferArch, tcb_src, dest);
}

/* ReadRegisters is a special case: replyFromKernel & setMRs are
 * unfolded here, in order to avoid passing the large reply message up
 * to the top level in a global (and double-copying). We prevent the
 * top-level replyFromKernel_success_empty() from running by setting the
 * thread state. Retype does this too.
 */
exception_t
invokeTCB_ReadRegisters(tcb_t *tcb_src, bool_t suspendSource,
                        word_t n, word_t arch, bool_t call)
{
    word_t i, j;
    exception_t e;
    tcb_t *thread;

    thread = ksCurThread;

    if (suspendSource) {
        suspend(tcb_src);
    }

    e = Arch_performTransfer(arch, tcb_src, ksCurThread);
    if (e != EXCEPTION_NONE) {
        return e;
    }

    if (call) {
        word_t *ipcBuffer;

        ipcBuffer = lookupIPCBuffer(true, thread);

        setRegister(thread, badgeRegister, 0);

        for (i = 0; i < n && i < n_frameRegisters && i < n_msgRegisters; i++) {
            setRegister(thread, msgRegisters[i],
                        getRegister(tcb_src, frameRegisters[i]));
        }

        if (ipcBuffer != NULL && i < n && i < n_frameRegisters) {
            for (; i < n && i < n_frameRegisters; i++) {
                ipcBuffer[i + 1] = getRegister(tcb_src, frameRegisters[i]);
            }
        }

        j = i;

        for (i = 0; i < n_gpRegisters && i + n_frameRegisters < n
                && i + n_frameRegisters < n_msgRegisters; i++) {
            setRegister(thread, msgRegisters[i + n_frameRegisters],
                        getRegister(tcb_src, gpRegisters[i]));
        }

        if (ipcBuffer != NULL && i < n_gpRegisters
                && i + n_frameRegisters < n) {
            for (; i < n_gpRegisters && i + n_frameRegisters < n; i++) {
                ipcBuffer[i + n_frameRegisters + 1] =
                    getRegister(tcb_src, gpRegisters[i]);
            }
        }

        setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                        seL4_MessageInfo_new(0, 0, 0, i + j)));
    }
    setThreadState(thread, ThreadState_Running);

    return EXCEPTION_NONE;
}

exception_t
invokeTCB_WriteRegisters(tcb_t *dest, bool_t resumeTarget,
                         word_t n, word_t arch, word_t *buffer)
{
    word_t i;
    word_t pc;
    exception_t e;

    e = Arch_performTransfer(arch, ksCurThread, dest);
    if (e != EXCEPTION_NONE) {
        return e;
    }

    if (n > n_frameRegisters + n_gpRegisters) {
        n = n_frameRegisters + n_gpRegisters;
    }

    for (i = 0; i < n_frameRegisters && i < n; i++) {
        /* Offset of 2 to get past the initial syscall arguments */
        setRegister(dest, frameRegisters[i],
                    sanitiseRegister(frameRegisters[i],
                                     getSyscallArg(i + 2, buffer)));
    }

    for (i = 0; i < n_gpRegisters && i + n_frameRegisters < n; i++) {
        setRegister(dest, gpRegisters[i],
                    sanitiseRegister(gpRegisters[i],
                                     getSyscallArg(i + n_frameRegisters + 2,
                                                   buffer)));
    }

    pc = getRestartPC(dest);
    setNextPC(dest, pc);

    if (resumeTarget) {
        restart(dest);
    }

    return EXCEPTION_NONE;
}

exception_t
invokeTCB_NotificationControl(tcb_t *tcb, notification_t *ntfnPtr)
{
    if (ntfnPtr) {
        bindNotification(tcb, ntfnPtr);
    } else {
        unbindNotification(tcb);
    }

    return EXCEPTION_NONE;
}

#ifdef DEBUG
void
setThreadName(tcb_t *tcb, const char *name)
{
    strlcpy(tcb->tcbName, name, TCB_NAME_LENGTH);
}
#endif
