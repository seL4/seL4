/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once


#define SMMU_SID_CNODE_SLOT_BITS    8
#define SMMU_CB_CNODE_SLOT_BITS     6
#define SID_INVALID      SMMU_MAX_SID
#define CB_INVALID       SMMU_MAX_CB
#define ASID_INVALID     nASIDs


exception_t decodeARMSIDControlInvocation(word_t label, unsigned int length, cptr_t cptr,
                                          cte_t *srcSlot, cap_t cap,
                                          bool_t call, word_t *buffer);

exception_t decodeARMSIDInvocation(word_t label, unsigned int length, cptr_t cptr,
                                   cte_t *srcSlot, cap_t cap, bool_t call, word_t *buffer);

exception_t decodeARMCBControlInvocation(word_t label, unsigned int length, cptr_t cptr,
                                         cte_t *srcSlot, cap_t cap,
                                         bool_t call, word_t *buffer);

exception_t decodeARMCBInvocation(word_t label, unsigned int length, cptr_t cptr,
                                  cte_t *srcSlot, cap_t cap, bool_t call, word_t *buffer);
exception_t smmu_delete_cb(cap_t cap);
exception_t smmu_delete_sid(cap_t cap);
void smmu_cb_delete_vspace(word_t cb, asid_t asid);
void invalidateSMMUTLBByASID(asid_t asid, word_t bind_cb);
void invalidateSMMUTLBByASIDVA(asid_t asid, vptr_t vaddr, word_t bind_cb);

