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

void configureIdleThread(tcb_t *tcb);
void activateThread(void) VISIBLE;
void suspend(tcb_t *target);
void restart(tcb_t *target);
void doIPCTransfer(tcb_t *sender, endpoint_t *endpoint,
                   word_t badge, bool_t grant, tcb_t *receiver,
                   bool_t diminish);
void doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot);
void doNormalTransfer(tcb_t *sender, word_t *sendBuffer, endpoint_t *endpoint,
                      word_t badge, bool_t canGrant, tcb_t *receiver,
                      word_t *receiveBuffer, bool_t diminish);
void doFaultTransfer(word_t badge, tcb_t *sender, tcb_t *receiver,
                     word_t *receiverIPCBuffer);
void doAsyncTransfer(word_t badge, word_t msgWord, tcb_t *thread);
void schedule(void);
void chooseThread(void);
void switchToThread(tcb_t *thread) VISIBLE;
void switchToIdleThread(void);
void setDomain(tcb_t *tptr, dom_t dom);
void setPriority(tcb_t *tptr, prio_t prio);
void scheduleTCB(tcb_t *tptr);
void attemptSwitchTo(tcb_t *tptr);
void switchIfRequiredTo(tcb_t *tptr);
void setThreadState(tcb_t *tptr, _thread_state_t ts) VISIBLE;
void timerTick(void);
void rescheduleRequired(void);

#endif
