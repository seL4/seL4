--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
-- Copyright 2024, Capabilities Limited
-- CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>
-- Default base size: uint64_t
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
base 64(48,0)
#else
base 64(48,1)
#endif
#define BF_CANONICAL_RANGE 48

-- Including the common structures_64.bf is neccessary because
-- we need the structures to be visible here when building
-- the capType
#include <object/structures_64.bf>

---- ARM-specific caps

block frame_cap (capFMappedASID, capFBasePtr, capFSize, capFMappedAddress,
capFVMRights, capFIsDevice, capType) {
    field capFMappedASID             16

#if defined(__CHERI_PURE_CAPABILITY__)
    padding                          48
#else
    field_high capFBasePtr           48
#endif

    field capType                    5
    field capFSize                   2
    field_high capFMappedAddress     48
    field capFVMRights               2
    field capFIsDevice               1
    padding                          6

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capFBasePtr            128
#endif
}

-- Page table caps
block page_table_cap (capPTMappedASID, capPTBasePtr, capPTIsMapped,
capPTMappedAddress, capType) {
    field capPTMappedASID            16

#if defined(__CHERI_PURE_CAPABILITY__)
    padding                          48
#else
    field_high capPTBasePtr          48
#endif

    field capType                    5
    padding                          10
    field capPTIsMapped              1
    field_high capPTMappedAddress    28
    padding                          20

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capPTBasePtr 128
#endif
}

-- First-level page table (vspace_root)
block vspace_cap(capVSMappedASID, capVSBasePtr, capVSIsMapped,
#ifdef CONFIG_ARM_SMMU
capVSMappedCB,
#endif
capType) {
    field capVSMappedASID            16

#if defined(__CHERI_PURE_CAPABILITY__)
    padding                          48

#else
    field_high capVSBasePtr          48
#endif

    field capType                    5
    field capVSIsMapped              1
#ifdef CONFIG_ARM_SMMU
    field capVSMappedCB              8
    padding                          50
#else
    padding                          58
#endif

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap capVSBasePtr 128
#endif
}

-- Cap to the table of 2^7 ASID pools
block asid_control_cap {
    padding                          64

    field capType                    5
    padding                          59

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 128
#endif
}

-- Cap to a pool of 2^9 ASIDs
block asid_pool_cap {
    padding                         64

    field capType                   5
    field capASIDBase               16
#if defined(__CHERI_PURE_CAPABILITY__)
    padding                         43
#else
    padding                         6
    field_high capASIDPool          37
#endif

#if defined(__CHERI_PURE_CAPABILITY__)
    cheri_cap  capASIDPool          128
#endif
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block vcpu_cap {
    padding                         64

    field      capType              5
#if defined(__CHERI_PURE_CAPABILITY__)
    padding                         48
    padding                         11
    cheri_cap capVCPUPtr 128
#else
    field_high capVCPUPtr           48
    padding                         11
#endif
}
#endif

#ifdef CONFIG_ARM_SMMU

block sid_control_cap {
    padding        64

    field capType  5
    padding        59

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 128
#endif
}

block sid_cap {

    padding              52
    field capSID         12

    field capType        5
    padding 59

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 128
#endif
}

block cb_control_cap {
    padding              64

    field capType        5
    padding              59

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 128
#endif
}


block cb_cap {

    padding               44
    field capBindSID      12
    field capCB           8


    field capType         5
    padding               59

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 128
#endif
}

#endif

#ifdef CONFIG_ALLOW_SMC_CALLS
block smc_cap {
    field capSMCBadge 64

    field capType  5
    padding        59

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 128
#endif
}
#endif

-- NB: odd numbers are arch caps (see isArchCap())
tagged_union cap capType {
    -- 5-bit tag caps
    tag null_cap                    0
    tag untyped_cap                 2
    tag endpoint_cap                4
    tag notification_cap            6
    tag reply_cap                   8
    tag cnode_cap                   10
    tag thread_cap                  12
    tag irq_control_cap             14
    tag irq_handler_cap             16
    tag zombie_cap                  18
    tag domain_cap                  20
#ifdef CONFIG_KERNEL_MCS
    tag sched_context_cap           22
    tag sched_control_cap           24
#endif

    -- 5-bit tag arch caps
    tag frame_cap                   1
    tag page_table_cap              3
    tag vspace_cap                  9
    tag asid_control_cap            11
    tag asid_pool_cap               13
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    tag vcpu_cap                    15
#endif
#ifdef CONFIG_ARM_SMMU
    tag sid_control_cap             17
    tag sid_cap                     19
    tag cb_control_cap              21
    tag cb_cap                      23
#endif
#ifdef CONFIG_ALLOW_SMC_CALLS
    tag smc_cap                     25
#endif
}

---- Arch-independent object types

block VMFault {
    field address                   64
    field FSR                       32
    field instructionFault          1
    padding                         27
    field seL4_FaultType            4
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block VGICMaintenance {
    padding          64

    field idx        6
    field idxValid   1
    padding         25
    padding         28
    field seL4_FaultType  4
}

block VCPUFault {
    padding         64
    field hsr       32
    padding         28
    field seL4_FaultType  4
}

block VPPIEvent {
    padding         55
    field irq_w      9
    padding         32
    padding         28
    field seL4_FaultType  4
}
#endif

block CHERIfault {
    field address                   64
    field FSR                       32
    field instructionFault          1
    padding                         27
    field seL4_FaultType            4
}

-- VM attributes

block vm_attributes {
    padding                         61
    field armExecuteNever           1
    field armParityEnabled          1
    field armPageCacheable          1
}

---- ARM-specific object types

block asid_map_none {
    padding                         63
    field type                      1

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 64
    padding 128
#endif
}

--- hw_vmids are required in hyp mode
block asid_map_vspace {
#ifdef CONFIG_ARM_SMMU
    field bind_cb                   8
    padding                         8
#else
    padding                         16
#endif

#if defined(__CHERI_PURE_CAPABILITY__)
    padding                         36
#else
    field_high vspace_root          36
#endif
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    padding                         2
    field stored_hw_vmid            8
    field stored_vmid_valid         1
#else
    padding                         11
#endif
    field type                      1

#if defined(__CHERI_PURE_CAPABILITY__)
    padding 64
    cheri_cap vspace_root           128
#endif
}

tagged_union asid_map type {
    tag asid_map_none 0
    tag asid_map_vspace 1
}

-- PGDE, PUDE, PDEs and PTEs, assuming 48-bit physical address
base 64(48,0)


-- See the definition of pte_type for explanation
-- for pte_sw_type and pte_hw_type
block pte_table {
#if defined(CONFIG_HAVE_CHERI)
    padding                         1
    field LC                        2
    field SC                        1
    field CDBM                      1
#else
    padding                         5
#endif
    field pte_sw_type               1
    padding                         10
    field_high pt_base_address      36
    padding                         10
    field pte_hw_type               2
}


-- The level 1 and 2 page pte structure
block pte_page {
#if defined(CONFIG_HAVE_CHERI)
    padding                         1
    field LC                        2
    field SC                        1
    field CDBM                      1
#else
    padding                         5
#endif
    field pte_sw_type               1
    padding                         3
    field UXN                       1
    padding                         6
    field_high page_base_address    36
    field nG                        1
    field AF                        1
    field SH                        2
    field AP                        2
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    field AttrIndx                  4
#else
    padding                         1
    field AttrIndx                  3
#endif
    field pte_hw_type               2
}

-- The level 3 page pte structure
block pte_4k_page {
#if defined(CONFIG_HAVE_CHERI)
    padding                         1
    field LC                        2
    field SC                        1
    field CDBM                      1
#else
    padding                         5
#endif
    field pte_sw_type               1
    padding                         3
    field UXN                       1
    padding                         6
    field_high page_base_address    36
    field nG                        1
    field AF                        1
    field SH                        2
    field AP                        2
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    field AttrIndx                  4
#else
    padding                         1
    field AttrIndx                  3
#endif
    field pte_hw_type               2
}

block pte_invalid {
    padding                         5
    field pte_sw_type               1
    padding                         56
    field pte_hw_type               2
}

-- There are two page type fields because the 4k page size
-- uses a different hardware encoding. We use bit 58
-- which is reserved for software use to encode this
-- difference in the tag for these types.
tagged_union pte pte_type(pte_hw_type, pte_sw_type) {
    tag pte_table               (3, 0)
    tag pte_page                (1, 0)
    tag pte_4k_page             (3, 1)
    tag pte_invalid             (0, 0)
}


block ttbr {
    field asid                      16
    field_high base_address         48
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#ifdef CONFIG_ARM_GIC_V3_SUPPORT
block virq_invalid {
    field virqType      2
    padding             1
    field virqGroup     1
    padding             4
    field virqPriority  8
    padding             3
    padding             3
    field virqEOIIRQEN  1
    padding             9
    field virqIRQ       32
}

block virq_active {
    field virqType      2
    padding             1
    field virqGroup     1
    padding             4
    field virqPriority  8
    padding             3
    padding             3
    field virqEOIIRQEN  1
    padding             9
    field virqIRQ       32
}

block virq_pending {
    field virqType      2
    padding             1
    field virqGroup     1
    padding             4
    field virqPriority  8
    padding             3
    padding             3
    field virqEOIIRQEN  1
    padding             9
    field virqIRQ       32
}
#else
block virq_invalid {
    padding             32
    padding             2
    field virqType      2
    padding             8
    field virqEOIIRQEN  1
    padding             19
}

block virq_active {
    padding             32
    padding             2
    field virqType      2
    padding             8
    field virqEOIIRQEN  1
    padding             19
}

block virq_pending {
    padding             32
    padding             1
    field virqGroup     1
    field virqType      2
    field virqPriority  5
    padding             3
    field virqEOIIRQEN  1
    padding             9
    field virqIRQ       10
}
#endif

tagged_union virq virqType {
    tag virq_invalid    0
    tag virq_pending    1
    tag virq_active     2
}
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#ifdef CONFIG_HARDWARE_DEBUG_API

block dbg_bcr {
    padding 34
    padding 1
    padding 5
    field breakpointType 4
    field lbn 4
    field ssc 2
    field hmc 1
    padding 4
    field bas 4
    padding 2
    field pmc 2
    field enabled 1
}

block dbg_wcr {
    padding 34
    padding 1
    field addressMask 5
    padding 3
    field watchpointType 1
    field lbn 4
    field ssc 2
    field hmc 1
    field bas 8
    field lsc 2
    field pac 2
    field enabled 1
}

#endif /* CONFIG_HARDWARE_DEBUG_API */

#include <sel4/arch/shared_types.bf>
