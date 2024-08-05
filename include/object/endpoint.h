/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <object/structures.h>

static inline tcb_queue_t PURE ep_ptr_get_queue(endpoint_t *epptr)
{
    tcb_queue_t queue;

    queue.head = (tcb_t *)endpoint_ptr_get_epQueue_head(epptr);
    queue.end = (tcb_t *)endpoint_ptr_get_epQueue_tail(epptr);

    return queue;
}

static inline void ep_ptr_set_queue(endpoint_t *epptr, tcb_queue_t queue)
{
    endpoint_ptr_set_epQueue_head(epptr, (word_t)queue.head);
    endpoint_ptr_set_epQueue_tail(epptr, (word_t)queue.end);
}

#ifdef CONFIG_KERNEL_MCS
void sendIPC(bool_t blocking, bool_t do_call, word_t badge,
             bool_t canGrant, bool_t canGrantReply, bool_t canDonate, tcb_t *thread,
             endpoint_t *epptr);
void receiveIPC(tcb_t *thread, cap_t cap, bool_t isBlocking, cap_t replyCPtr);
void reorderEP(endpoint_t *epptr, tcb_t *thread);
#else
void sendIPC(bool_t blocking, bool_t do_call, word_t badge,
             bool_t canGrant, bool_t canGrantReply, tcb_t *thread,
             endpoint_t *epptr);
void receiveIPC(tcb_t *thread, cap_t cap, bool_t isBlocking);
#endif
void cancelIPC(tcb_t *tptr);
void cancelAllIPC(endpoint_t *epptr);
void cancelBadgedSends(endpoint_t *epptr, word_t badge);
void replyFromKernel_error(tcb_t *thread);
void replyFromKernel_success_empty(tcb_t *thread);

