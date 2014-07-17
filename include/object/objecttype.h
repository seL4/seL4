/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_OBJECTTYPE_H
#define __OBJECT_OBJECTTYPE_H

#include <types.h>
#include <api/failures.h>
#include <object/cnode.h>
#include <object/structures.h>
#include <plat/machine/hardware.h>

struct deriveCap_ret {
    exception_t status;
    cap_t cap;
};
typedef struct deriveCap_ret deriveCap_ret_t;

struct finaliseCap_ret {
    cap_t remainder;
    irq_t irq;
};
typedef struct finaliseCap_ret finaliseCap_ret_t;

#include <arch/object/objecttype.h>

deriveCap_ret_t deriveCap(cte_t *slot, cap_t cap);
finaliseCap_ret_t finaliseCap(cap_t cap, bool_t final, bool_t exposed);
cap_t recycleCap(bool_t is_final, cap_t cap);
bool_t CONST hasRecycleRights(cap_t cap);
bool_t CONST sameRegionAs(cap_t cap_a, cap_t cap_b);
bool_t CONST sameObjectAs(cap_t cap_a, cap_t cap_b);
cap_t CONST updateCapData(bool_t preserve, word_t newData, cap_t cap);
cap_t CONST maskCapRights(cap_rights_t cap_rights, cap_t cap);
cap_t createObject(object_t t, void *regionBase, word_t);
void createNewObjects(object_t t, cte_t *parent, slot_range_t slots,
                      void *regionBase, word_t userSize);
exception_t decodeInvocation(word_t label, unsigned int length,
                             cptr_t capIndex, cte_t *slot, cap_t cap,
                             extra_caps_t extraCaps, bool_t block, bool_t call,
                             word_t *buffer);
exception_t performInvocation_Endpoint(endpoint_t *ep, word_t badge,
                                       bool_t canGrant, bool_t block,
                                       bool_t call);
exception_t performInvocation_AsyncEndpoint(async_endpoint_t *aep,
                                            word_t badge, word_t message);
exception_t performInvocation_Reply(tcb_t *thread, cte_t *slot);
word_t getObjectSize(word_t t, word_t userObjSize);

#endif
