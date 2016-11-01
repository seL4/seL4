/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#ifndef __ARCH_MODE_KERNEL_TLB_BITMAP_H_
#define __ARCH_MODE_KERNEL_TLB_BITMAP_H_

#include <kernel/vspace.h>
#include <plat_mode/machine/hardware.h>
#include <machine.h>

#if CONFIG_MAX_NUM_NODES > 1
/* Bit zero for entries in PD are present bits and should always be 0 */
#define TLBBITMAP_ENTRIES_PER_PD        31

/* Number of entries in PD reserved for TLB bitmap */
#define TLBBITMAP_PD_ENTRIES            (((CONFIG_MAX_NUM_NODES - 1) / TLBBITMAP_ENTRIES_PER_PD) + 1)
#define TLBBITMAP_PD_RESERVED           (TLBBITMAP_PD_ENTRIES * BIT(LARGE_PAGE_BITS))
#define TLBBITMAP_PD_INDEX              (TLBBITMAP_PPTR >> LARGE_PAGE_BITS)

/* Total number of usable bits in TLBbitmap */
#define TLBBITMAP_PD_BITS               (TLBBITMAP_ENTRIES_PER_PD * TLBBITMAP_PD_ENTRIES)

#define TLBBITMAP_PD_MAKE_INDEX(_cpu)   ((_cpu) / (wordBits - 1))
#define TLBBITMAP_PD_MAKE_BIT(_cpu)     BIT(((_cpu) % (wordBits - 1)) + 1)

static inline void
tlb_bitmap_init(pde_t *pd)
{
    for (int i = 0; i < TLBBITMAP_PD_ENTRIES; i++) {
        pd[TLBBITMAP_PD_INDEX + i] = makeUserPDEPageTableInvalid();
    }
}

static inline void
tlb_bitmap_set(pde_t *pd, word_t cpu)
{
    assert(cpu < TLBBITMAP_PD_BITS && cpu <= wordBits);
    pd[TLBBITMAP_PD_MAKE_INDEX(cpu)].words[0] |= TLBBITMAP_PD_MAKE_BIT(cpu);
}

static inline void
tlb_bitmap_unset(pde_t *pd, word_t cpu)
{
    assert(cpu < TLBBITMAP_PD_BITS && cpu <= wordBits);
    pd[TLBBITMAP_PD_MAKE_INDEX(cpu)].words[0] &= ~TLBBITMAP_PD_MAKE_BIT(cpu);
}

static inline word_t
tlb_bitmap_get(pde_t *pd)
{
    word_t bitmap;

    for (int i = 0; i < TLBBITMAP_PD_ENTRIES; i++) {
        word_t entry = pd[TLBBITMAP_PD_INDEX].words[0];
        // skip present bit
        entry >>= 1;

        int shift = i * TLBBITMAP_ENTRIES_PER_PD;
        assert((entry & MASK(shift)) == entry);
        bitmap |= entry << shift;
    }
    return bitmap;
}

#else
#define TLBBITMAP_PD_RESERVED            0
#endif /* CONFIG_MAX_NUM_NODES */

#endif /* __ARCH_MODE_KERNEL_TLB_BITMAP_H_ */
