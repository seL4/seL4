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

#include <arch/machine.h>
#include <plat/machine.h>
#include <machine/registerset.h>
#include <machine/hardware.h>

/* platform independent functions */

static inline void *CONST
paddr_to_pptr(paddr_t paddr)
{
    return (void *)(paddr + BASE_OFFSET);
}

static inline paddr_t CONST
pptr_to_paddr(void *pptr)
{
    return (paddr_t)pptr - BASE_OFFSET;
}

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

static inline bool_t CONST
inKernelWindow(void *pptr)
{
    paddr_t paddr = pptr_to_paddr(pptr);
    return ((paddr >= PADDR_BASE) && (paddr < PADDR_TOP));
}


#endif
