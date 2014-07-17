/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARMV_FASTPATH_H__
#define __ARMV_FASTPATH_H__

#include <arch/object/structures.h>
#include <arch/api/types.h>
#include <arch/types.h>
#include <plat/machine/hardware.h>
#include <arch/machine.h>

/* Change the translation root by updating TTBR0. */
static inline void
setCurrentPD_fp(word_t pd_addr)
{
    pd_addr &= 0xffffe000;
    pd_addr |= 0x18;

    asm("mcr p15, 0, %0, c2, c0, 0" : : "r"(pd_addr));
    asm("isb");
}

static inline void
armv_contextSwitch_fp(pde_t* cap_pd, hw_asid_t hw_asid)
{
    /* ASID/PD synchronisation achieved through PD with no non-global mappings */
    setCurrentPD_fp(addrFromPPtr(armKSGlobalPD));
    setHardwareASID(hw_asid);
    setCurrentPD_fp(addrFromPPtr(cap_pd));
}


#endif /* __ARMV_FASTPATH_H__ */
