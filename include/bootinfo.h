/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __BOOTINFO_H
#define __BOOTINFO_H

#include <config.h>
#include <types.h>
#include <api/bootinfo_types.h>

#define BI_PTR(r) ((seL4_BootInfo*)(r))
#define BI_REF(p) ((word_t)(p))
#define BI_FRAME_SIZE_BITS PAGE_BITS
#define S_REG_EMPTY (seL4_SlotRegion){ .start = 0, .end = 0 }

/* adjust constants in config.h if this assert fails */
compile_assert(bi_size, sizeof(seL4_BootInfo) <= BIT(BI_FRAME_SIZE_BITS))

#endif
