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

#include <arch/types.h>
#include <arch/linker.h>

enum vm_fault_type {
    IA32DataFault = 0,
    IA32InstructionFault = 1
};
typedef uint32_t vm_fault_type_t;

enum vm_page_size {
    IA32_4K,
    IA32_4M
};
typedef uint32_t vm_page_size_t;

enum frameSizeConstants {
    IA32_4K_bits = 12,
    IA32_4M_bits = 22
};

#define PAGE_BITS 12

/* Any changes to this function need to be replicated in pageBitsForSize_phys.
 */
static inline unsigned int CONST
pageBitsForSize(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case IA32_4K:
        return IA32_4K_bits;

    case IA32_4M:
        return IA32_4M_bits;

    default:
        fail("Invalid page size");
    }
}

/* This function is a duplicate of pageBitsForSize, needed for calls that occur
 * before the MMU is turned on. Note that any changes to this function need to
 * be replicated in pageBitsForSize.
 */
PHYS_CODE
static inline unsigned int CONST
pageBitsForSize_phys(vm_page_size_t pagesize)
{
    switch (pagesize) {
    case IA32_4K:
        return IA32_4K_bits;

    case IA32_4M:
        return IA32_4M_bits;

    default:
        fail("Invalid page size");
    }
}

/* Returns the size of CPU's cacheline */
uint32_t CONST getCacheLineSize(void);
uint32_t CONST getCacheLineSizeBits(void);

/* Flushes a specific memory range from the CPU cache */
void flushCacheLine(void* vaddr);
void flushCacheRange(void* vaddr, uint32_t size_bits);

#endif
