/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MACHINE_H
#define __MACHINE_H

#include <plat/machine.h>
#include <machine/registerset.h>
#include <machine/hardware.h>

static inline void* CONST
ptrFromPAddr(paddr_t paddr)
{
    return (void*)(paddr + physMappingOffset);
}

static inline paddr_t CONST
addrFromPPtr(void* pptr)
{
    return (paddr_t)pptr - physMappingOffset;
}

static inline region_t CONST
paddr_to_pptr_reg(p_region_t p_reg)
{
    return (region_t) {
        p_reg.start + physMappingOffset, p_reg.end + physMappingOffset
    };
}

static inline p_region_t CONST
pptr_to_paddr_reg(region_t reg)
{
    return (p_region_t) {
        reg.start - physMappingOffset, reg.end - physMappingOffset
    };
}

#define paddr_to_pptr ptrFromPAddr
#define pptr_to_paddr(x) addrFromPPtr(x)

#endif
