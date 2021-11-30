--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
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

block frame_cap {
    field capFMappedASID             16
    field_high capFBasePtr           48

    field capType                    5
    field capFSize                   2
    field_high capFMappedAddress     48
    field capFVMRights               2
    field capFIsDevice               1
    padding                          6
}

-- Page table caps
block page_table_cap {
    field capPTMappedASID            16
    field_high capPTBasePtr          48

    field capType                    5
    padding                          10
    field capPTIsMapped              1
    field_high capPTMappedAddress    28
    padding                          20
}

-- First-level page table (vspace_root)
block vspace_cap {
    field capMappedASID              16
    field_high capPTBasePtr          48

    field capType                    5
    field capIsMapped                1
#ifdef CONFIG_ARM_SMMU 
    field capMappedCB                8
    padding                          50
#else 
    padding                          58
#endif 
}

-- Cap to the table of 2^7 ASID pools
block asid_control_cap {
    padding                          64

    field capType                    5
    padding                          59
}

-- Cap to a pool of 2^9 ASIDs
block asid_pool_cap {
    padding                         64

    field capType                   5
    field capASIDBase               16
    padding                         6
    field_high capASIDPool          37
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block vcpu_cap {
    padding                         64

    field      capType              5
    field_high capVCPUPtr           48
    padding                         11
}
#endif

#ifdef CONFIG_ARM_SMMU

block sid_control_cap {
    padding        64

    field capType  5
    padding        59
}

block sid_cap {

    padding              52
    field capSID         12

    field capType        5
    padding 59
}

block cb_control_cap {
    padding              64

    field capType        5
    padding              59
}


block cb_cap {

    padding               44
    field capBindSID      12
    field capCB           8


    field capType         5
    padding               59
}

#endif

#ifdef CONFIG_ALLOW_SMC_CALLS
block smc_cap {
    field capSMCBadge 64

    field capType  5
    padding        59
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
    field irq_w     64
    padding         32
    padding         28
    field seL4_FaultType  4
}
#endif

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
}

--- hw_vmids are required in hyp mode
block asid_map_vspace {
#ifdef CONFIG_ARM_SMMU
    field bind_cb                   8
    padding                         8
#else
    padding                         16
#endif
    field_high vspace_root          36
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    padding                         2
    field stored_hw_vmid            8
    field stored_vmid_valid         1
#else
    padding                         11
#endif
    field type                      1
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
    padding                         5
    field pte_sw_type               1
    padding                         10
    field_high pt_base_address      36
    padding                         10
    field pte_hw_type               2
}


-- The level 1 and 2 page pte structure
block pte_page {
    padding                         5
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
    padding                         5
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

#include <sel4/arch/shared_types.bf>
