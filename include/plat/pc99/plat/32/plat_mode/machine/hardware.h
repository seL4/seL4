/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MODE_MACHINE_HARDWARE_H
#define __PLAT_MODE_MACHINE_HARDWARE_H

/* WARNING: some of these constants are also defined in linker.lds */
#define PADDR_BASE  0x00000000
#define PADDR_LOAD  0x00100000
#define PPTR_BASE   0xe0000000
#ifdef CONFIG_PAE_PAGING
#define PPTR_USER_TOP (PPTR_BASE & (~MASK(X86_1G_bits)))
#else
#define PPTR_USER_TOP (PPTR_BASE & (~MASK(X86_4M_bits)))
#endif
#ifdef CONFIG_ENABLE_BENCHMARKS
#define PPTR_TOP    (-BIT(LARGE_PAGE_BITS + 1))
#else
#define PPTR_TOP    (-BIT(LARGE_PAGE_BITS))
#endif /* CONFIG_ENABLE_BENCHMARKS */
#define PPTR_KDEV   0xffff0000
#define BASE_OFFSET (PPTR_BASE - PADDR_BASE)
#define PADDR_TOP   (PPTR_TOP - BASE_OFFSET)
/* The kernel base offset is a way to translate the kernel image segment
 * from virtual to physical. This translation must be a single offset for
 * for the entire segment (i.e. the kernel image must be contiguous both
 * virtually and physically) */
#define KERNEL_BASE_OFFSET BASE_OFFSET

static inline void* CONST
paddr_to_pptr(paddr_t paddr)
{
    return (void*)(paddr + BASE_OFFSET);
}

static inline paddr_t CONST
pptr_to_paddr(void* pptr)
{
    return (paddr_t)pptr - BASE_OFFSET;
}

/* For a 32-bit system there is no difference in how we translates
 * physical address for the kernel symbols or anything else */
#define paddr_to_kpptr(x) paddr_to_pptr(x)
#define kpptr_to_paddr(x) pptr_to_paddr(x)

static inline region_t CONST
paddr_to_pptr_reg(p_region_t p_reg)
{
    return (region_t) {
        p_reg.start + BASE_OFFSET, p_reg.end + BASE_OFFSET
    };
}

static inline p_region_t CONST
pptr_to_paddr_reg(region_t reg)
{
    return (p_region_t) {
        reg.start - BASE_OFFSET, reg.end - BASE_OFFSET
    };
}

#endif
