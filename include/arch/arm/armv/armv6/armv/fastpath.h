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

static inline void dsb_fp(void)
{
    /*
     * Perform a data synchronisation barrier to be performed when we change
     * address spaces.
     */
    asm volatile (
        "mcr p15, 0, %[zero], c7, c10, 4\n\t"
        :
        : [zero]"r"(0)
    );
}

/* Change the translation root by updating TTBR0. */
static inline void
setCurrentPD_fp(word_t pd_addr)
{

    asm volatile (
        "mcr p15, 0, %[pd_addr], c2, c0, 0\n\t" /* Write TTBR0. */
        :
        : [pd_addr]"r"(pd_addr | 0x18)
    );
}

/* Change the current hardware ASID. */
static inline void
setHardwareASID_fp(hw_asid_t asid)
{
    asm volatile (
        "mcr p15, 0, %[ctx], c13, c0, 1\n\t" /* Write context ID. */
        "mcr p15, 0, %[zero], c7, c5, 6\n\t" /* Flush BTAC. */
        :
        : [ctx]"r"(asid),
        [zero]"r"(0)
    );
}

static inline void
armv_contextSwitch_fp(pde_t* cap_pd, hw_asid_t hw_asid)
{
    /* No ASID/PD synchronisation required */
    dsb_fp();
    setCurrentPD_fp(addrFromPPtr(cap_pd));
    setHardwareASID_fp(hw_asid);
}


#endif /* __ARMV_FASTPATH_H__ */
