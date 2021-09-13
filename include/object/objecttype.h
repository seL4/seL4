/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/cnode.h>
#include <object/structures.h>
#include <plat/machine/hardware.h>
#include <object/cap.h>
#include <arch/object/objecttype.h>
#include <object/interrupt.h>

deriveCap_ret_t deriveCap(cte_t *slot, cap_t cap);
finaliseCap_ret_t finaliseCap(cap_t cap, bool_t final, bool_t exposed);
bool_t CONST hasCancelSendRights(cap_t cap);
bool_t CONST sameRegionAs(cap_t cap_a, cap_t cap_b);
bool_t CONST sameObjectAs(cap_t cap_a, cap_t cap_b);
cap_t CONST updateCapData(bool_t preserve, word_t newData, cap_t cap);
cap_t CONST maskCapRights(seL4_CapRights_t seL4_CapRights, cap_t cap);
cap_t createObject(object_t t, void *regionBase, word_t, bool_t deviceMemory);
void createNewObjects(object_t t, cte_t *parent,
                      cte_t *destCNode, word_t destOffset, word_t destLength,
                      void *regionBase, word_t userSize, bool_t deviceMemory);
#ifdef CONFIG_KERNEL_MCS
exception_t decodeInvocation(word_t invLabel, word_t length,
                             cptr_t capIndex, cte_t *slot, cap_t cap,
                             bool_t block, bool_t call,
                             bool_t canDonate, bool_t firstPhase, word_t *buffer);
exception_t performInvocation_Endpoint(endpoint_t *ep, word_t badge,
                                       bool_t canGrant, bool_t canGrantReply,
                                       bool_t block, bool_t call, bool_t canDonate);
exception_t performInvocation_Notification(notification_t *ntfn,
                                           word_t badge);
exception_t performInvocation_Reply(tcb_t *thread, reply_t *reply, bool_t canGrant);
#else
exception_t decodeInvocation(word_t invLabel, word_t length,
                             cptr_t capIndex, cte_t *slot, cap_t cap,
                             bool_t block, bool_t call, word_t *buffer);
exception_t performInvocation_Endpoint(endpoint_t *ep, word_t badge,
                                       bool_t canGrant, bool_t canGrantReply,
                                       bool_t block, bool_t call);
exception_t performInvocation_Notification(notification_t *ntfn,
                                           word_t badge);
exception_t performInvocation_Reply(tcb_t *thread, cte_t *slot, bool_t canGrant);
#endif
word_t getObjectSize(word_t t, word_t userObjSize);

static inline void postCapDeletion(cap_t cap)
{
    if (cap_get_capType(cap) == cap_irq_handler_cap) {
        irq_t irq = IDX_TO_IRQT(cap_irq_handler_cap_get_capIRQ(cap));
        deletedIRQHandler(irq);
    } else if (isArchCap(cap)) {
        Arch_postCapDeletion(cap);
    }
}

word_t CONST cap_get_capSizeBits(cap_t cap);
bool_t CONST cap_get_capIsPhysical(cap_t cap);
void *CONST cap_get_capPtr(cap_t cap);
bool_t CONST isCapRevocable(cap_t derivedCap, cap_t srcCap);
