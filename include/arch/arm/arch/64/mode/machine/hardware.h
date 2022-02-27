/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <sel4/sel4_arch/constants.h>

#define PAGE_BITS seL4_PageBits

/* Extract the n-level PT index from a virtual address:
 * - n is page table level counting from the root page table,
 * - NUM_PT_LEVELS are either 3 or 4 page table levels depending
 *   on whether the address range being translated is 48bits, 44 bits or 40 bits.
 * - If translating an address in the kernel addrspace NUM_PT_LEVELS = 4 always.
 * - PageTables always have 512 slots (PT_INDEX_BITS = 9) but if there are only
 *   3 total levels then the root level is implemented with 4 concatenated tables
 *   meaning 2048 slots (UPT_LEVELS = 3 => seL4_VSpaceIndexBits = 12)
 *
 * PT_LEVEL_SHIFT(n) == PT_INDEX_BITS * (NUM_PT_LEVELS - n) + seL4_PageBits
 * GET_PT_INDEX(addr, n) == (addr >> PT_LEVEL_SHIFT(n)) & MASK(PT_INDEX_BITS)
 */
#ifdef AARCH64_VSPACE_S2_START_L1
#define UPT_LEVELS 3
#define ULVL_FRM_ARM_PT_LVL(n) ((n)-1)
#else
#define UPT_LEVELS 4
#define ULVL_FRM_ARM_PT_LVL(n) (n)
#endif
#define KPT_LEVELS 4
#define KLVL_FRM_ARM_PT_LVL(n) (n)

#define KPT_LEVEL_SHIFT(n) (((PT_INDEX_BITS) * (((KPT_LEVELS) - 1) - (n))) + seL4_PageBits)
#define GET_KPT_INDEX(addr, n)  (((addr) >> KPT_LEVEL_SHIFT(n)) & MASK(PT_INDEX_BITS))
#define GET_KLVL_PGSIZE(n)      BIT(KPT_LEVEL_SHIFT((n)))

#define UPT_LEVEL_SHIFT(n) (((PT_INDEX_BITS) * (((UPT_LEVELS) - 1) - (n))) + seL4_PageBits)
#define UPT_INDEX_MASK(n) (n == 0 ? seL4_VSpaceIndexBits : PT_INDEX_BITS)
#define GET_UPT_INDEX(addr, n)  (((addr) >> UPT_LEVEL_SHIFT(n)) & MASK(UPT_INDEX_MASK(n)))
#define GET_ULVL_PGSIZE_BITS(n) UPT_LEVEL_SHIFT((n))
#define GET_ULVL_PGSIZE(n)      BIT(UPT_LEVEL_SHIFT((n)))


/* Control register fields */
#define CONTROL_M         0  /* MMU enable */
#define CONTROL_A         1  /* Alignment check enable */
#define CONTROL_C         2  /* Cacheability control, for data caching */
#define CONTROL_SA0       4  /* Stack Alignment Check Enable for EL0 */
#define CONTROL_SA        3  /* Stack Alignment Check for EL1 */
#define CONTROL_I         12 /* Instruction access Cacheability control */
#define CONTROL_UCT       15 /* Enable EL0 access to CTR_EL0   */
#define CONTROL_E0E       24 /* Endianness of data accesses at EL0 */
#define CONTROL_EE        25 /* Endianness of data accesses at EL1 */
#define CONTROL_UCI       26 /* Trap EL0 execution of cache maintenance
                                instructions to EL1 (aarch64 only) */

#ifndef __ASSEMBLER__

#include <arch/types.h>

enum vm_page_size {
    ARMSmallPage,
    ARMLargePage,
    ARMHugePage
};
typedef word_t vm_page_size_t;

enum frameSizeConstants {
    ARMSmallPageBits    = seL4_PageBits,
    ARMLargePageBits    = seL4_LargePageBits,
    ARMHugePageBits     = seL4_HugePageBits
};

static inline word_t CONST pageBitsForSize(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case ARMSmallPage:
        return ARMSmallPageBits;

    case ARMLargePage:
        return ARMLargePageBits;

    case ARMHugePage:
        return ARMHugePageBits;

    default:
        fail("Invalid page size");
    }
}

#endif /* __ASSEMBLER__ */

