/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_KERNEL_TLB_BITMAP_DEFS_H_
#define __ARCH_KERNEL_TLB_BITMAP_DEFS_H_

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

#endif /* __ARCH_KERNEL_TLB_BITMAP_DEFS_H_ */
