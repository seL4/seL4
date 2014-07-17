/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_ASYNCENDPOINT_H
#define __OBJECT_ASYNCENDPOINT_H

#include <types.h>
#include <object/structures.h>

void sendAsyncIPC(async_endpoint_t *aepptr, word_t badge, word_t val);
void receiveAsyncIPC(tcb_t *thread, cap_t cap);
void aepCancelAll(async_endpoint_t *aepptr);
void asyncIPCCancel(tcb_t *threadPtr, async_endpoint_t *aepptr);

#endif
