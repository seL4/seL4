/*
 * Copyright 2021, DornerWorks Ltd.
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define NUM_SMC_REGS 8

exception_t decodeARMSMCInvocation(word_t label, unsigned int length, cptr_t cptr,
                                   cte_t *srcSlot, cap_t cap, bool_t call, word_t *buffer);
