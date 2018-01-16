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

#ifndef __ARCH_KERNEL_TLB_BITMAP_H_
#define __ARCH_KERNEL_TLB_BITMAP_H_

#include <config.h>
#include <types.h>
#include <mode/types.h>
#include <plat_mode/machine/hardware.h>
#include <arch/kernel/tlb_bitmap_defs.h>
#include <mode/kernel/vspace.h>

#ifdef ENABLE_SMP_SUPPORT

static inline void
tlb_bitmap_init(vspace_root_t *root)
{
    for (int i = 0; i < TLBBITMAP_ROOT_ENTRIES; i++) {
        root[TLBBITMAP_ROOT_INDEX + i] = x86_make_empty_root_mapping();
    }
}

static inline void
tlb_bitmap_set(vspace_root_t *root, word_t cpu)
{
    assert(cpu < TLBBITMAP_ROOT_BITS && cpu <= wordBits);
    root[TLBBITMAP_ROOT_MAKE_INDEX(cpu)].words[0] |= TLBBITMAP_ROOT_MAKE_BIT(cpu);
}

static inline void
tlb_bitmap_unset(vspace_root_t *root, word_t cpu)
{
    assert(cpu < TLBBITMAP_ROOT_BITS && cpu <= wordBits);
    root[TLBBITMAP_ROOT_MAKE_INDEX(cpu)].words[0] &= ~TLBBITMAP_ROOT_MAKE_BIT(cpu);
}

static inline word_t
tlb_bitmap_get(vspace_root_t *root)
{
    word_t bitmap = 0;

    for (int i = 0; i < TLBBITMAP_ROOT_ENTRIES; i++) {
        word_t entry = root[TLBBITMAP_ROOT_INDEX + i].words[0];
        // skip present bit
        entry >>= 1;

        int shift = i * TLBBITMAP_ENTRIES_PER_ROOT;
        bitmap |= entry << shift;
    }
    return bitmap;
}

#else
#define TLBBITMAP_ROOT_ENTRIES 0
#endif /* ENABLE_SMP_SUPPORT */

#endif /* __ARCH_KERNEL_TLB_BITMAP_H_ */
