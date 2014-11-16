/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <assert.h>

#include <types.h>
#include <kernel/thread.h>
#include <object/structures.h>
#include <object/tcb.h>
#include <object/endpoint.h>
#include <model/statedata.h>
#include <machine/io.h>

#include <object/asyncendpoint.h>

static inline tcb_queue_t PURE
aep_ptr_get_queue(async_endpoint_t *aepptr)
{
    tcb_queue_t aep_queue;

    aep_queue.head = (tcb_t*)async_endpoint_ptr_get_aepQueue_head(aepptr);
    aep_queue.end = (tcb_t*)async_endpoint_ptr_get_aepQueue_tail(aepptr);

    return aep_queue;
}

static inline void
aep_ptr_set_queue(async_endpoint_t *aepptr, tcb_queue_t aep_queue)
{
    async_endpoint_ptr_set_aepQueue_head(aepptr, (word_t)aep_queue.head);
    async_endpoint_ptr_set_aepQueue_tail(aepptr, (word_t)aep_queue.end);
}

static inline void
aep_set_active(async_endpoint_t *aepptr, word_t badge)
{
    async_endpoint_ptr_set_state(aepptr, AEPState_Active);
    async_endpoint_ptr_set_aepMsgIdentifier(aepptr, badge);
}


void
sendAsyncIPC(async_endpoint_t *aepptr, word_t badge)
{
    switch (async_endpoint_ptr_get_state(aepptr)) {
    case AEPState_Idle: {
        tcb_t *tcb = (tcb_t*)async_endpoint_ptr_get_aepBoundTCB(aepptr);
        /* Check if we are bound and that thread is waiting for a message */
        if (tcb) {
            if (thread_state_ptr_get_tsType(&tcb->tcbState) == ThreadState_BlockedOnReceive) {
                /* Send and start thread running */
                ipcCancel(tcb);
                setThreadState(tcb, ThreadState_Running);
                setRegister(tcb, badgeRegister, badge);
                attemptSwitchTo(tcb);
            } else {
                aep_set_active(aepptr, badge);
            }
        } else {
            aep_set_active(aepptr, badge);
        }
        break;
    }
    case AEPState_Waiting: {
        tcb_queue_t aep_queue;
        tcb_t *dest;

        aep_queue = aep_ptr_get_queue(aepptr);
        dest = aep_queue.head;

        /* Haskell error "WaitingAEP AEP must have non-empty queue" */
        assert(dest);

        /* Dequeue TCB */
        aep_queue = tcbEPDequeue(dest, aep_queue);
        aep_ptr_set_queue(aepptr, aep_queue);

        /* set the thread state to idle if the queue is empty */
        if (!aep_queue.head) {
            async_endpoint_ptr_set_state(aepptr, AEPState_Idle);
        }

        setThreadState(dest, ThreadState_Running);
        setRegister(dest, badgeRegister, badge);
        switchIfRequiredTo(dest);
        break;
    }

    case AEPState_Active: {
        word_t badge2;

        badge2 = async_endpoint_ptr_get_aepMsgIdentifier(aepptr);
        badge2 |= badge;

        async_endpoint_ptr_set_aepMsgIdentifier(aepptr, badge2);
        break;
    }
    }
}

void
receiveAsyncIPC(tcb_t *thread, cap_t cap, bool_t isBlocking)
{
    async_endpoint_t *aepptr;

    aepptr = AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(cap));

    switch (async_endpoint_ptr_get_state(aepptr)) {
    case AEPState_Idle:
        /* Fall through */
    case AEPState_Waiting: {
        tcb_queue_t aep_queue;

        if (isBlocking) {
            /* Block thread on endpoint */
            thread_state_ptr_set_tsType(&thread->tcbState,
                                        ThreadState_BlockedOnAsyncEvent);
            thread_state_ptr_set_blockingIPCEndpoint(&thread->tcbState,
                                                     AEP_REF(aepptr));
            scheduleTCB(thread);

            /* Enqueue TCB */
            aep_queue = aep_ptr_get_queue(aepptr);
            aep_queue = tcbEPAppend(thread, aep_queue);

            async_endpoint_ptr_set_state(aepptr, AEPState_Waiting);
            aep_ptr_set_queue(aepptr, aep_queue);
        } else {
            doPollFailedTransfer(thread);
        }
        break;
    }

    case AEPState_Active:
        setRegister(
            thread, badgeRegister,
            async_endpoint_ptr_get_aepMsgIdentifier(aepptr));
        async_endpoint_ptr_set_state(aepptr, AEPState_Idle);
        break;
    }
}

void
aepCancelAll(async_endpoint_t *aepptr)
{
    if (async_endpoint_ptr_get_state(aepptr) == AEPState_Waiting) {
        tcb_t *thread = TCB_PTR(async_endpoint_ptr_get_aepQueue_head(aepptr));

        async_endpoint_ptr_set_state(aepptr, AEPState_Idle);
        async_endpoint_ptr_set_aepQueue_head(aepptr, 0);
        async_endpoint_ptr_set_aepQueue_tail(aepptr, 0);

        /* Set all waiting threads to Restart */
        for (; thread; thread = thread->tcbEPNext) {
            setThreadState(thread, ThreadState_Restart);
            tcbSchedEnqueue(thread);
        }
        rescheduleRequired();
    }
}

void
asyncIPCCancel(tcb_t *threadPtr, async_endpoint_t *aepptr)
{
    tcb_queue_t aep_queue;

    /* Haskell error "asyncIPCCancel: async endpoint must be waiting" */
    assert(async_endpoint_ptr_get_state(aepptr) == AEPState_Waiting);

    /* Dequeue TCB */
    aep_queue = aep_ptr_get_queue(aepptr);
    aep_queue = tcbEPDequeue(threadPtr, aep_queue);
    aep_ptr_set_queue(aepptr, aep_queue);

    /* Make endpoint idle */
    if (!aep_queue.head) {
        async_endpoint_ptr_set_state(aepptr, AEPState_Idle);
    }

    /* Make thread inactive */
    setThreadState(threadPtr, ThreadState_Inactive);
}

void
completeAsyncIPC(async_endpoint_t *aepptr, tcb_t *tcb)
{
    word_t badge;

    if (likely(tcb && async_endpoint_ptr_get_state(aepptr) == AEPState_Active)) {
        async_endpoint_ptr_set_state(aepptr, AEPState_Idle);
        badge = async_endpoint_ptr_get_aepMsgIdentifier(aepptr);
        setRegister(tcb, badgeRegister, badge);
    } else {
        fail("tried to complete async ipc with inactive AEP");
    }
}

void
unbindAsyncEndpoint(tcb_t *tcb)
{
    async_endpoint_t *aepptr;
    aepptr = tcb->boundAsyncEndpoint;

    if (aepptr) {
        async_endpoint_ptr_set_aepBoundTCB(aepptr, (word_t) 0);
        tcb->boundAsyncEndpoint = NULL;
    }
}

void
bindAsyncEndpoint(tcb_t *tcb, async_endpoint_t *aepptr)
{
    async_endpoint_ptr_set_aepBoundTCB(aepptr, (word_t)tcb);
    tcb->boundAsyncEndpoint = aepptr;
}


