/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_HARDWARE_H
#define __ARCH_MACHINE_HARDWARE_H

#include <types.h>
#include <config.h>
#include <arch/types.h>
#include <arch/linker.h>

enum vm_fault_type {
    X86DataFault = 0,
    X86InstructionFault = 1
};
typedef word_t vm_fault_type_t;

enum vm_page_size {
    IA32_SmallPage,
    IA32_LargePage,
    IA32_HugePage
};
typedef word_t vm_page_size_t;

enum vm_page_map_type {
    X86_MAPPING_NONE = 0,
    X86_MAPPING_VSPACE,
    X86_MAPPING_IOSPACE
};
typedef uint32_t vm_page_map_type_t;

enum frameSizeConstants {
    X86_4K_bits = 12,
    X86_2M_bits = 21,
    X86_4M_bits = 22,
    X86_1G_bits = 30
};

#define PAGE_BITS       X86_4K_bits
#define PAGE_1G_BITS    X86_1G_bits

#if defined(CONFIG_PAE_PAGING) || defined(X86_64)
#define LARGE_PAGE_BITS X86_2M_bits
#else
#define LARGE_PAGE_BITS X86_4M_bits
#endif

#define HUGE_PAGE_BITS  X86_1G_bits


/* Any changes to this function need to be replicated in pageBitsForSize_phys.
 */
static inline word_t CONST
pageBitsForSize(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case IA32_SmallPage:
        return PAGE_BITS;

    case IA32_LargePage:
        return LARGE_PAGE_BITS;

    case IA32_HugePage:
        return HUGE_PAGE_BITS;

    default:
        fail("Invalid page size");
    }
}

/* This function is a duplicate of pageBitsForSize, needed for calls that occur
 * before the MMU is turned on. Note that any changes to this function need to
 * be replicated in pageBitsForSize.
 */
PHYS_CODE
static inline word_t CONST
pageBitsForSize_phys(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case IA32_SmallPage:
        return PAGE_BITS;

    case IA32_LargePage:
        return LARGE_PAGE_BITS;

    default:
        fail("Invalid page size");
    }
}

/* Returns the size of CPU's cacheline */
uint32_t CONST getCacheLineSize(void);
uint32_t CONST getCacheLineSizeBits(void);

/* Flushes a specific memory range from the CPU cache */
static inline void flushCacheLine(void* vaddr)
{
    asm volatile("clflush %[vaddr]" :: [vaddr] "m"(vaddr));
}

void flushCacheRange(void* vaddr, uint32_t size_bits);

/* Disables a variety of prefetchers */
bool_t disablePrefetchers(void);

#endif
