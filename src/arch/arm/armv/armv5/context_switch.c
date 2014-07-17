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
#include <arch/machine.h>

void armv_contextSwitch(pde_t* cap_pd)
{
    cleanCache();
    setCurrentPD(addrFromPPtr(cap_pd));
    invalidateTLB();
    setCurrentASID(cap_pd);
}


