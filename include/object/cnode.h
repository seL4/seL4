/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_CNODE_H
#define __OBJECT_CNODE_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

struct slot_range {
    cte_t *cnode;
    unsigned int offset;
    unsigned int length;
};
typedef struct slot_range slot_range_t;

exception_t decodeCNodeInvocation(word_t label, unsigned int length,
                                  cap_t cap, extra_caps_t extraCaps,
                                  word_t *buffer);
exception_t invokeCNodeRevoke(cte_t *destSlot);
exception_t invokeCNodeDelete(cte_t *destSlot);
exception_t invokeCNodeRecycle(cte_t *destSlot);
exception_t invokeCNodeInsert(cap_t cap, cte_t *srcSlot, cte_t *destSlot);
exception_t invokeCNodeMove(cap_t cap, cte_t *srcSlot, cte_t *destSlot);
exception_t invokeCNodeRotate(cap_t cap1, cap_t cap2, cte_t *slot1,
                              cte_t *slot2, cte_t *slot3);
exception_t invokeCNodeSaveCaller(cte_t *destSlot);
void cteInsert(cap_t newCap, cte_t *srcSlot, cte_t *destSlot);
void cteMove(cap_t newCap, cte_t *srcSlot, cte_t *destSlot);
void capSwapForDelete(cte_t *slot1, cte_t *slot2);
void cteSwap(cap_t cap1, cte_t *slot1, cap_t cap2, cte_t *slot2);
exception_t cteRevoke(cte_t *slot);
exception_t cteDelete(cte_t *slot, bool_t exposed);
void cteDeleteOne(cte_t* slot);
exception_t cteRecycle(cte_t *slot);
void insertNewCap(cte_t *parent, cte_t *slot, cap_t cap);
void setupReplyMaster(tcb_t *thread);
bool_t PURE isMDBParentOf(cte_t *cte_a, cte_t *cte_b);
exception_t ensureNoChildren(cte_t *slot);
exception_t ensureEmptySlot(cte_t *slot);
bool_t PURE isFinalCapability(cte_t *cte);
bool_t PURE slotCapLongRunningDelete(cte_t *slot);
cte_t *getReceiveSlots(tcb_t *thread, word_t *buffer);
cap_transfer_t PURE loadCapTransfer(word_t *buffer);

#endif
