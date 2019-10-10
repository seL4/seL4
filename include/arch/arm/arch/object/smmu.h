/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once


#define SMMU_SID_CNODE_SLOT_BITS    8
#define SMMU_CB_CNODE_SLOT_BITS     6

exception_t decodeARMSIDControlInvocation(word_t label, unsigned int length, cptr_t cptr,
                                          cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
                                          bool_t call, word_t *buffer);

exception_t decodeARMSIDInvocation(word_t label, unsigned int length, cptr_t cptr,
                                   cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
                                   bool_t call, word_t *buffer);

exception_t decodeARMCBControlInvocation(word_t label, unsigned int length, cptr_t cptr,
                                         cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
                                         bool_t call, word_t *buffer);

exception_t decodeARMCBInvocation(word_t label, unsigned int length, cptr_t cptr,
                                  cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
                                  bool_t call, word_t *buffer);
