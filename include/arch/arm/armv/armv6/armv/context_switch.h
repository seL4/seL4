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
#include <mode/model/statedata.h>

static inline void setHardwareASID(hw_asid_t hw_asid)
{
    dsb();
    flushBTAC();
    writeContextID(hw_asid);
}

static inline void armv_contextSwitch_HWASID(pde_t* cap_pd, hw_asid_t hw_asid)
{
    setCurrentPD(addrFromPPtr(cap_pd));
    setHardwareASID(hw_asid);
}

static inline void armv_contextSwitch(pde_t* cap_pd, asid_t asid)
{
    armv_contextSwitch_HWASID(cap_pd, getHWASID(asid));
}

#endif /* __ARMV_CONTEXT_SWITCH_H__ */
