/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_CSPACE_H
#define __KERNEL_CSPACE_H

#include <types.h>
#include <api/failures.h>
#include <api/types.h>
#include <object/structures.h>

struct lookupCap_ret {
    exception_t status;
    cap_t cap;
};
typedef struct lookupCap_ret lookupCap_ret_t;

struct lookupCapAndSlot_ret {
    exception_t status;
    cap_t cap;
    cte_t *slot;
};
typedef struct lookupCapAndSlot_ret lookupCapAndSlot_ret_t;

struct lookupSlot_raw_ret {
    exception_t status;
    cte_t *slot;
};
typedef struct lookupSlot_raw_ret lookupSlot_raw_ret_t;

struct lookupSlot_ret {
    exception_t status;
    cte_t *slot;
};
typedef struct lookupSlot_ret lookupSlot_ret_t;

struct resolveAddressBits_ret {
    exception_t status;
    cte_t *slot;
    unsigned int bitsRemaining;
};
typedef struct resolveAddressBits_ret resolveAddressBits_ret_t;

lookupCap_ret_t lookupCap(tcb_t *thread, cptr_t cPtr);
lookupCapAndSlot_ret_t lookupCapAndSlot(tcb_t *thread, cptr_t cPtr);
lookupSlot_raw_ret_t lookupSlot(tcb_t *thread, cptr_t capptr);
lookupSlot_ret_t lookupSlotForCNodeOp(bool_t isSource,
                                      cap_t root, cptr_t capptr,
                                      unsigned int depth);
lookupSlot_ret_t lookupSourceSlot(cap_t root, cptr_t capptr,
                                  unsigned int depth);
lookupSlot_ret_t lookupTargetSlot(cap_t root, cptr_t capptr,
                                  unsigned int depth);
lookupSlot_ret_t lookupPivotSlot(cap_t root, cptr_t capptr,
                                 unsigned int depth);
resolveAddressBits_ret_t resolveAddressBits(cap_t nodeCap,
                                            cptr_t capptr,
                                            unsigned int n_bits);

#endif
