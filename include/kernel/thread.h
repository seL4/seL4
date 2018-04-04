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

static inline CONST word_t
invert_l1index(word_t l1index)
{
    word_t inverted = (L2_BITMAP_SIZE - 1 - l1index);
    assert(inverted < L2_BITMAP_SIZE);
    return inverted;
}

static inline prio_t
getHighestPrio(word_t dom)
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

static inline bool_t
isHighestPrio(word_t dom, prio_t prio)
{
    return NODE_STATE(ksReadyQueuesL1Bitmap)[dom] == 0 ||
           prio >= getHighestPrio(dom);
}

void configureIdleThread(tcb_t *tcb);
void activateThread(void);
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
void switchToThread(tcb_t *thread);
void switchToIdleThread(void);
void setDomain(tcb_t *tptr, dom_t dom);
void setPriority(tcb_t *tptr, prio_t prio);
void setMCPriority(tcb_t *tptr, prio_t mcp);
void scheduleTCB(tcb_t *tptr);
void possibleSwitchTo(tcb_t *tptr);
void setThreadState(tcb_t *tptr, _thread_state_t ts);
void timerTick(void);
void rescheduleRequired(void);

/* declare that the thread has had its registers (in its user_context_t) modified and it
 * should ignore any 'efficient' restores next time it is run, and instead restore all
 * registers into their correct place */
void Arch_postModifyRegisters(tcb_t *tptr);

#endif
