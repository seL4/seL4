/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <arch/object/structures.h>
#include <arch/api/types.h>
#include <mode/model/statedata.h>

static inline void setHardwareASID(hw_asid_t hw_asid)
{
    dsb();
    flushBTAC();
    writeContextID(hw_asid);
}

static inline void armv_contextSwitch_HWASID(pde_t *cap_pd, hw_asid_t hw_asid)
{
    setCurrentPD(addrFromPPtr(cap_pd));
    setHardwareASID(hw_asid);
}

static inline void armv_contextSwitch(pde_t *cap_pd, asid_t asid)
{
    armv_contextSwitch_HWASID(cap_pd, getHWASID(asid));
}

