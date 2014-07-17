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
#include <model/statedata.h>

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

void
sendAsyncIPC(async_endpoint_t *aepptr, word_t badge, word_t val)
{
    switch (async_endpoint_ptr_get_state(aepptr)) {
    case AEPState_Idle:
        async_endpoint_ptr_set_state(aepptr, AEPState_Active);
        async_endpoint_ptr_set_aepMsgIdentifier(aepptr, badge);
        async_endpoint_ptr_set_aepData(aepptr, val);
        break;

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

        if (!aep_queue.head) {
            async_endpoint_ptr_set_state(aepptr, AEPState_Idle);
        }

        setThreadState(dest, ThreadState_Running);
        doAsyncTransfer(badge, val, dest);
        switchIfRequiredTo(dest);
        break;
    }

    case AEPState_Active: {
        word_t badge2, val2;

        val2 = async_endpoint_ptr_get_aepData(aepptr);
        val2 |= val;

        badge2 = async_endpoint_ptr_get_aepMsgIdentifier(aepptr);
        badge2 |= badge;

        async_endpoint_ptr_set_aepMsgIdentifier(aepptr, badge2);
        async_endpoint_ptr_set_aepData(aepptr, val2);
        break;
    }
    }
}

void
receiveAsyncIPC(tcb_t *thread, cap_t cap)
{
    async_endpoint_t *aepptr;

    aepptr = AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(cap));

    switch (async_endpoint_ptr_get_state(aepptr)) {
    case AEPState_Idle:
    case AEPState_Waiting: {
        tcb_queue_t aep_queue;

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
        break;
    }

    case AEPState_Active:
        doAsyncTransfer(
            async_endpoint_ptr_get_aepMsgIdentifier(aepptr),
            async_endpoint_ptr_get_aepData(aepptr), thread);
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
