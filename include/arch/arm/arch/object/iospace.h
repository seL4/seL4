/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#ifdef CONFIG_TK1_SMMU

seL4_SlotRegion create_iospace_caps(cap_t root_cnode_cap);
exception_t decodeARMIOPTInvocation(word_t invLabel, uint32_t length, cte_t *slot, cap_t cap, word_t *buffer);
exception_t decodeARMIOMapInvocation(word_t invLabel, uint32_t length, cte_t *slot, cap_t cap, word_t *buffer);
exception_t performPageInvocationUnmapIO(cap_t cap, cte_t *slot);
exception_t decodeARMIOSpaceInvocation(word_t invLabel, cap_t cap);
void unmapIOPage(cap_t cap);
void deleteIOPageTable(cap_t cap);
void clearIOPageDirectory(cap_t cap);

#else

/* define dummy functions */
static inline seL4_SlotRegion create_iospace_caps(cap_t root_cnode_cap)
{
    return S_REG_EMPTY;
}

static inline exception_t decodeARMIOPTInvocation(word_t invLabel, uint32_t length, cte_t *slot, cap_t cap,
                                                  word_t *buffer)
{
    return EXCEPTION_NONE;
}

static inline exception_t decodeARMIOMapInvocation(word_t invLabel, uint32_t length, cte_t *slot, cap_t cap,
                                                   word_t *buffer)
{
    return EXCEPTION_NONE;
}

static inline exception_t performPageInvocationUnmapIO(cap_t cap, cte_t *slot)
{
    return EXCEPTION_NONE;
}

static inline exception_t decodeARMIOSpaceInvocation(word_t invLabel, cap_t cap)
{
    return EXCEPTION_NONE;
}

static inline void unmapIOPage(cap_t cap)
{
}

static inline void deleteIOPageTable(cap_t cap)
{
}

static inline void clearIOPageDirectory(cap_t cap)
{
}

#endif /* end of !CONFIG_TK1_SMMU */


