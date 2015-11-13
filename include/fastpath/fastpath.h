/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __FASTPATH_H
#define __FASTPATH_H

/* Fastpath cap lookup.  Returns a null_cap on failure. */
static inline cap_t FORCE_INLINE
lookup_fp(cap_t cap, cptr_t cptr)
{
    word_t cptr2;
    cte_t *slot;
    word_t radixBits, bits;
    word_t radix;

    if (unlikely(! cap_capType_equals(cap, cap_cnode_cap))) {
        return cap_null_cap_new();
    }

    bits = cap_cnode_cap_get_capCNodeGuardSize(cap);
    do {
        radixBits = cap_cnode_cap_get_capCNodeRadix(cap);
        cptr2 = cptr << bits;
        radix = cptr2 >> (32 - radixBits);
        slot = CTE_PTR(cap_cnode_cap_get_capCNodePtr(cap)) + radix;

        cap = slot->cap;
        bits += radixBits;
    } while (unlikely(bits < wordBits) && likely(cap_capType_equals(cap, cap_cnode_cap)));

    return cap;
}

static inline void
thread_state_ptr_set_tsType_np(thread_state_t *ts_ptr, word_t tsType)
{
    ts_ptr->words[0] = tsType;
}

static inline void
thread_state_ptr_mset_blockingObject_tsType(thread_state_t *ts_ptr,
                                            word_t ep_ref,
                                            word_t tsType)
{
    ts_ptr->words[0] = ep_ref | tsType;
}

static inline void
cap_reply_cap_ptr_new_np(cap_t *cap_ptr, word_t capCallerSlot)
{
    /* 1 is capReplyMaster */
    cap_ptr->words[1] = CTE_REF(capCallerSlot) | 1;
    cap_ptr->words[0] = cap_reply_cap;
}

static inline void
cap_reply_cap_ptr_new_np2(cap_t *cap_ptr, word_t isMaster, word_t capTCBPtr)
{
    cap_ptr->words[0] = TCB_REF(capTCBPtr) | cap_reply_cap;
    cap_ptr->words[1] = isMaster;
}

static inline void
endpoint_ptr_mset_epQueue_tail_state(endpoint_t *ep_ptr, word_t epQueue_tail,
                                     word_t state)
{
    ep_ptr->words[0] = epQueue_tail | state;
}

static inline void
endpoint_ptr_set_epQueue_head_np(endpoint_t *ep_ptr, word_t epQueue_head)
{
    ep_ptr->words[1] = epQueue_head;
}

#include <arch/fastpath/fastpath.h>

#endif
