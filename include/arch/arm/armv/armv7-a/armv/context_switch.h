/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */
#ifndef __ARMV_CONTEXT_SWITCH_H__
#define __ARMV_CONTEXT_SWITCH_H__

#include <arch/object/structures.h>
#include <arch/api/types.h>

/** MODIFIES: [*] */
static inline void setHardwareASID(hw_asid_t hw_asid)
{
#if defined(CONFIG_ARM_ERRATA_430973)
    flushBTAC();
#endif
    writeContextID(hw_asid);
}

static inline void armv_contextSwitch_HWASID(pde_t *cap_pd, hw_asid_t hw_asid)
{
    /*
     * On ARMv7, speculative refills that complete between switching
     * ASID and PD can cause TLB entries to be Tagged with the wrong
     * ASID. The correct method to avoid this problem is to
     * either cycle the context switch through a reserved ASID or
     * through a page directory that has only global mappings.
     * The reserved Page directory method has shown to perform better
     * than the reserved ASID method.
     *
     * We do not call setCurrentPD here as we want to perform a
     * minimal number of DSB and ISBs and the second PD switch we
     * do does not need a DSB
     */
    dsb();
    writeTTBR0(addrFromPPtr(armKSGlobalPD));
    isb();
    setHardwareASID(hw_asid);
    writeTTBR0(addrFromPPtr(cap_pd));
    isb();
}

static inline void armv_contextSwitch(pde_t* pd)
{
    armv_contextSwitch_HWASID(pd, getHWASID(pd));
}

#endif /* __ARMV_CONTEXT_SWITCH_H__ */
