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

#ifndef __ARCH_MACHINE_HARDWARE_64_H
#define __ARCH_MACHINE_HARDWARE_64_H

#include <config.h>
#include <mode/api/constants.h>

#define PAGE_BITS seL4_PageBits

#if defined(CONFIG_ARM_CORTEX_A53)
#define L1_CACHE_LINE_SIZE_BITS  6 /* 64 bytes */
#endif

#if defined(CONFIG_ARM_CORTEX_A57)
#define L1_CACHE_LINE_SIZE_BITS  6 /* 64 bytes */
#endif

#ifndef L1_CACHE_LINE_SIZE_BITS
#error Unable to determine L1 cache line size
#endif

#define L1_CACHE_LINE_SIZE BIT(L1_CACHE_LINE_SIZE_BITS)

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

static inline word_t CONST
pageBitsForSize(vm_page_size_t pagesize)
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

#endif /* __ARCH_MACHINE_HARDWARE_64_H */
