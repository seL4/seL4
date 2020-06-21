/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <basic_types.h>
#include <config.h>
#include <arch/types.h>
#include <linker.h>
#include <sel4/sel4_arch/constants.h>

#define PAGE_BITS seL4_PageBits
#define LARGE_PAGE_BITS seL4_LargePageBits
#define L1_CACHE_LINE_SIZE_BITS CTZL(CONFIG_CACHE_LN_SZ)
#define L1_CACHE_LINE_SIZE CONFIG_CACHE_LN_SZ

enum vm_fault_type {
    X86DataFault = seL4_DataFault,
    X86InstructionFault = seL4_InstructionFault
};

typedef word_t vm_fault_type_t;

enum vm_page_size {
    X86_SmallPage,
    X86_LargePage,
    X64_HugePage
};
typedef word_t vm_page_size_t;

enum frameSizeConstants {
    X64SmallPageBits = seL4_PageBits,
    X64LargePageBits = seL4_LargePageBits,
    X64HugePageBits  = seL4_HugePageBits
};

enum vm_page_map_type {
    X86_MappingNone = 0,
    X86_MappingVSpace,
#ifdef CONFIG_IOMMU
    X86_MappingIOSpace,
#endif
#ifdef CONFIG_VTX
    X86_MappingEPT
#endif
};
typedef word_t vm_page_map_type_t;

/* Any changes to this function need to be replicated in pageBitsForSize_phys.
 */
static inline word_t CONST pageBitsForSize(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case X86_SmallPage:
        return seL4_PageBits;

    case X86_LargePage:
        return seL4_LargePageBits;

    case X64_HugePage:
        return seL4_HugePageBits;

    default:
        fail("Invalid page size");
    }
}

/* This function is a duplicate of pageBitsForSize, needed for calls that occur
 * before the MMU is turned on. Note that any changes to this function need to
 * be replicated in pageBitsForSize.
 */
PHYS_CODE
static inline word_t CONST pageBitsForSize_phys(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case X86_SmallPage:
        return seL4_PageBits;

    case X86_LargePage:
        return seL4_LargePageBits;

    case X64_HugePage:
        return seL4_HugePageBits;

    default:
        fail("Invalid page size");
    }
}

/* Returns the size of CPU's cacheline */
uint32_t CONST getCacheLineSize(void);
uint32_t CONST getCacheLineSizeBits(void);

/* Flushes a specific memory range from the CPU cache */
static inline void flushCacheLine(volatile void *vaddr)
{
    asm volatile("clflush %[vaddr]" : [vaddr] "+m"(*((volatile char *)vaddr)));
}

void flushCacheRange(void *vaddr, uint32_t size_bits);

/* Disables a variety of prefetchers */
bool_t disablePrefetchers(void);

/* Enable user level access to the performance counters */
BOOT_CODE void enablePMCUser(void);

/* Flushes entire CPU Cache */
static inline void x86_wbinvd(void)
{
    asm volatile("wbinvd" ::: "memory");
}

static inline void arch_clean_invalidate_caches(void)
{
    x86_wbinvd();
}

/* Initialize Indirect Branch Restricted Speculation into the mode specified by the build configuration */
bool_t init_ibrs(void);
