/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <armv/context_switch.h>

/** MODIFIES: [*] */
void setHardwareASID(hw_asid_t hw_asid)
{
#if defined(CONFIG_ARM_ERRATA_430973)
    flushBTAC();
#endif
    writeContextID(hw_asid);
}

void armv_contextSwitch(pde_t* cap_pd, asid_t asid)
{
    /*
     * On ARMv7, speculative refills that complete between switching
     * ASID and PD can cause TLB entries to be Tagged with the wrong
     * ASID. The correct method to avoid this problem is to
     * either cycle the context switch through a reserved ASID or
     * through a page directory that has only global mappings.
     * The reserved Page directory method has shown to perform better
     * than the reserved ASID method.
     */
    setCurrentPD(addrFromPPtr(armKSGlobalPD));
    setCurrentASID(asid);
    setCurrentPD(addrFromPPtr(cap_pd));
}


