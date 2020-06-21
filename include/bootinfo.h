/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <sel4/bootinfo_types.h>

#define BI_PTR(r) ((seL4_BootInfo*)(r))
#define BI_REF(p) ((word_t)(p))
#define BI_FRAME_SIZE_BITS PAGE_BITS
#define S_REG_EMPTY (seL4_SlotRegion){ .start = 0, .end = 0 }

/* adjust constants in config.h if this assert fails */
compile_assert(bi_size, sizeof(seL4_BootInfo) <= BIT(BI_FRAME_SIZE_BITS))

