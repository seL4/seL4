/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <sel4/sel4_arch/constants.h>

#define PAGE_BITS seL4_PageBits

/* Control register fields */
#define CONTROL_M         0  /* MMU enable */
#define CONTROL_A         1  /* Alignment check enable */
#define CONTROL_C         2  /* Cacheability control, for data caching */
#define CONTROL_SA0       4  /* Stack Alignment Check Enable for EL0 */
#define CONTROL_SA        3  /* Stack Alignment Check for EL1 */
#define CONTROL_I         12 /* Instruction access Cacheability control */
#define CONTROL_E0E       24 /* Endianness of data accesses at EL0 */
#define CONTROL_EE        25 /* Endianness of data accesses at EL1 */

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

