/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <plat/machine.h>
#include <machine/registerset.h>
#include <mode/hardware.h>
#include <mode/machine.h>
#include <plat/machine/hardware.h>

static inline void *CONST ptrFromPAddr(paddr_t paddr)
{
    return (void *)(paddr + BASE_OFFSET);
}

static inline paddr_t CONST addrFromPPtr(void *pptr)
{
    return (paddr_t)pptr - BASE_OFFSET;
}

static inline region_t CONST paddr_to_pptr_reg(p_region_t p_reg)
{
    return (region_t) {
        p_reg.start + BASE_OFFSET, p_reg.end + BASE_OFFSET
    };
}

static inline p_region_t CONST pptr_to_paddr_reg(region_t reg)
{
    return (p_region_t) {
        reg.start - BASE_OFFSET, reg.end - BASE_OFFSET
    };
}

#define paddr_to_pptr ptrFromPAddr
#define pptr_to_paddr(x) addrFromPPtr(x)

