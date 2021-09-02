/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <sel4/bootinfo_types.h>

/* declare object-specific macros to hide the casting */
#define BI_PTR(r) ((seL4_BootInfo*)(r))
#define BI_REF(p) ((word_t)(p))

#define S_REG_EMPTY (seL4_SlotRegion){ .start = 0, .end = 0 }

/* The boot info frame takes at least one page, it must be big enough to hold
 * the seL4_BootInfo data structure. Due to internal restrictions, the boot info
 * frame size must be of the form 2^n. Furthermore, there might still be code
 * that makes the hard-coded assumption the boot info frame is always one page.
 */
#define BI_FRAME_SIZE_BITS PAGE_BITS
compile_assert(bi_size, sizeof(seL4_BootInfo) <= BIT(BI_FRAME_SIZE_BITS))
