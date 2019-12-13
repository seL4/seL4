/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_ENDPOINT_H
#define __OBJECT_ENDPOINT_H

#include <types.h>
#include <object/structures.h>

static inline tcb_queue_t PURE ep_ptr_get_queue(endpoint_t *epptr)
{
    tcb_queue_t queue;

    queue.head = (tcb_t *)endpoint_ptr_get_epQueue_head(epptr);
    queue.end = (tcb_t *)endpoint_ptr_get_epQueue_tail(epptr);

    return queue;
}

void sendIPC(bool_t blocking, bool_t do_call, word_t badge,
             bool_t canGrant, bool_t canGrantReply, tcb_t *thread,
             endpoint_t *epptr);
#ifdef CONFIG_KERNEL_MCS
void receiveIPC(tcb_t *thread, cap_t cap, bool_t isBlocking, cap_t replyCPtr);
void reorderEP(endpoint_t *epptr, tcb_t *thread);
#else
void receiveIPC(tcb_t *thread, cap_t cap, bool_t isBlocking);
#endif
void cancelIPC(tcb_t *tptr);
void cancelAllIPC(endpoint_t *epptr);
void cancelBadgedSends(endpoint_t *epptr, word_t badge);
void replyFromKernel_error(tcb_t *thread);
void replyFromKernel_success_empty(tcb_t *thread);

#endif
