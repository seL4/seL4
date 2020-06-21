/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <plat/machine.h>
#include <machine/registerset.h>
#include <hardware.h>

/* When translating a physical address into an address accessible to the
 * kernel via virtual addressing we always use the mapping of the memory
 * into the physical memory window, even if the mapping originally
 * referred to a kernel virtual address. */
static inline void *CONST ptrFromPAddr(paddr_t paddr)
{
    return (void *)(paddr + PPTR_BASE_OFFSET);
}

/* When obtaining a physical address from a reference to any object in
 * the physical mapping window, this function must be used. */
static inline paddr_t CONST addrFromPPtr(void *pptr)
{
    return (paddr_t)pptr - PPTR_BASE_OFFSET;
}

static inline region_t CONST paddr_to_pptr_reg(p_region_t p_reg)
{
    return (region_t) {
        p_reg.start + PPTR_BASE_OFFSET, p_reg.end + PPTR_BASE_OFFSET
    };
}

static inline p_region_t CONST pptr_to_paddr_reg(region_t reg)
{
    return (p_region_t) {
        reg.start - PPTR_BASE_OFFSET, reg.end - PPTR_BASE_OFFSET
    };
}

#define paddr_to_pptr ptrFromPAddr
#define pptr_to_paddr(x) addrFromPPtr(x)

#include <mode/machine.h>
