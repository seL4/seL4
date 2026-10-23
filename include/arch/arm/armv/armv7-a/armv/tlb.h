/*
 * Copyright 2026, Proofcraft Pty Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <machine.h>
#include <mode/model/statedata.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

/* In hyp mode, TLBIALL in invalidateLocalTLB() only invalidates entries for the
 * current VMID, so we need to switch to the target VMID first if it is not the
 * current one. Use the global empty user vspace as a valid intermediate table
 * address that does not add any mappings.
 */
static inline void invalidateLocalTLB_VMID(hw_asid_t vmid)
{
    uint64_t vttbr = getVTTBR();
    hw_asid_t current_vmid = (vttbr >> 48) & MASK(hwASIDBits);
    if (current_vmid != vmid) {
        /* writeContextIDAndPD() is the convenience form of setTTBR() with
         * separate VMID and PD arguments. */
        writeContextIDAndPD(vmid, addrFromKPPtr(armUSGlobalPD));
    }
    invalidateLocalTLB();
    if (current_vmid != vmid) {
        setVTTBR(vttbr);
    }
}

/* Interface function. In hyp mode, hw_asid_t is a VMID. */
static inline void invalidateLocalTLB_ASID(hw_asid_t hw_asid)
{
    invalidateLocalTLB_VMID(hw_asid);
}

static inline void invalidateLocalTLB_VAASID(word_t mva_plus_asid)
{
    /* Stage 2 entries cannot be invalidated by MVA from hyp mode, so invalidate
     * all entries of the VMID. */
    invalidateLocalTLB_VMID(mva_plus_asid & MASK(hwASIDBits));
}

#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
