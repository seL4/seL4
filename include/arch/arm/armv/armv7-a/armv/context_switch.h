/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/object/structures.h>
#include <arch/api/types.h>
#include <mode/model/statedata.h>

static inline void setHardwareASID(hw_asid_t hw_asid)
{
#if defined(CONFIG_ARM_ERRATA_430973)
    flushBTAC();
#endif
    writeContextID(hw_asid);
}

static inline void armv_contextSwitch_HWASID(pde_t *cap_pd, hw_asid_t hw_asid)
{
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    writeContextIDAndPD(hw_asid, addrFromPPtr(cap_pd));
#else
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
    writeTTBR0Ptr(addrFromKPPtr(armKSGlobalPD));
    isb();
    setHardwareASID(hw_asid);
    writeTTBR0Ptr(addrFromPPtr(cap_pd));
    isb();
#endif
}

static inline void armv_contextSwitch(pde_t *cap_pd, asid_t asid)
{
    armv_contextSwitch_HWASID(cap_pd, getHWASID(asid));
}

