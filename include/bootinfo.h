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
