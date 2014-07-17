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

void armv_contextSwitch(pde_t* cap_pd, asid_t asid)
{
    setCurrentPD(addrFromPPtr(cap_pd));
    setCurrentASID(asid);
}

/** MODIFIES: [*] */
void setHardwareASID(hw_asid_t hw_asid)
{
    dsb();
    flushBTAC();
    writeContextID(hw_asid);
}
