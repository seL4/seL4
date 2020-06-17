/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef ENABLE_SMP_SUPPORT
/* Bit zero for entries in PD are present bits and should always be 0 */
#define TLBBITMAP_ENTRIES_PER_ROOT (wordBits - 1)

/* Number of entries in PD reserved for TLB bitmap */
#define TLBBITMAP_ROOT_ENTRIES (((CONFIG_MAX_NUM_NODES - 1) / TLBBITMAP_ENTRIES_PER_ROOT) + 1)
#define TLBBITMAP_RESERVED_VSPACE (TLBBITMAP_ROOT_ENTRIES * BIT(TLBBITMAP_ROOT_ENTRY_SIZE))
#define TLBBITMAP_ROOT_INDEX GET_VSPACE_ROOT_INDEX(TLBBITMAP_PPTR)

/* Total number of usable bits in TLBbitmap */
#define TLBBITMAP_ROOT_BITS (TLBBITMAP_ENTRIES_PER_ROOT * TLBBITMAP_ROOT_ENTRIES)

#define TLBBITMAP_ROOT_MAKE_INDEX(_cpu) (TLBBITMAP_ROOT_INDEX + ((_cpu) / TLBBITMAP_ENTRIES_PER_ROOT))
#define TLBBITMAP_ROOT_MAKE_BIT(_cpu) BIT(((_cpu) % TLBBITMAP_ENTRIES_PER_ROOT) + 1)

#else
#define TLBBITMAP_ROOT_ENTRIES 0
#endif /* ENABLE_SMP_SUPPORT */


