/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <plat/machine.h>
#include <machine/registerset.h>
#include <hardware.h>

#if defined(__CHERI_PURE_CAPABILITY__)
#include <mode/cheri.h>
/* We trust the kernel to only use this powerful KernelVirtOffsetCap only
 * when it needs to convert from physical to virtual addresses (and vice
 * versa) and access them later on e.g., when iterating over page tables.
 */
#define KERNEL_PPTR_OFFSET KernelVirtOffsetCap
#else
#define KERNEL_PPTR_OFFSET PPTR_BASE_OFFSET
#endif

/* When translating a physical address into an address accessible to the
 * kernel via virtual addressing we always use the mapping of the memory
 * into the physical memory window, even if the mapping originally
 * referred to a kernel virtual address. */
static inline void *CONST ptrFromPAddr(paddr_t paddr)
{
    return (void *)(paddr + KERNEL_PPTR_OFFSET);
}

/* When obtaining a physical address from a reference to any object in
 * the physical mapping window, this function must be used. */
static inline paddr_t CONST addrFromPPtr(const void *pptr)
{
    return (paddr_t)pptr - PPTR_BASE_OFFSET;
}

/* When obtaining a physical address from a reference to an address from
 * the kernel ELF mapping, this function must be used. */
static inline paddr_t CONST addrFromKPPtr(const void *pptr)
{
    assert((paddr_t)pptr >= KERNEL_ELF_BASE);
    assert((paddr_t)pptr <= KERNEL_ELF_TOP);
    return (paddr_t)pptr - KERNEL_ELF_BASE_OFFSET;
}

#define paddr_to_pptr(x)   ptrFromPAddr((uintptr_t) x)
#define pptr_to_paddr(x)   addrFromPPtr(x)
#define kpptr_to_paddr(x)  addrFromKPPtr(x)

static inline region_t CONST paddr_to_pptr_reg(const p_region_t p_reg)
{
    region_t pptr_reg = (region_t) {
        .start = (pptr_t)paddr_to_pptr(p_reg.start),
        .end   = (pptr_t)paddr_to_pptr(p_reg.end)
    };

#if defined(__CHERI_PURE_CAPABILITY__)
    /* Bound the returned region with CHERI capabilities */
    pptr_reg.start = (pptr_t) cheri_derive_data_cap((void *) pptr_reg.start, (ptraddr_t) pptr_reg.start, p_reg.end - p_reg.start, -1);
    pptr_reg.end = pptr_reg.start + (p_reg.end - p_reg.start);
#endif

    return pptr_reg;
}

static inline p_region_t CONST pptr_to_paddr_reg(const region_t reg)
{
    return (p_region_t) {
        .start = pptr_to_paddr((const void *)reg.start),
        .end   = pptr_to_paddr((const void *)reg.end),
    };
}


#include <mode/machine.h>
