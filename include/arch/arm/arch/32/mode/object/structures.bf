--
-- Copyright 2014, General Dynamics C4 Systems
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>
-- Default base size: uint32_t
base 32

-- Including the common structures_32.bf is neccessary because
-- we need the structures to be visible here when building
-- the capType
#include <object/structures_32.bf>

---- ARM-specific caps

-- 4k frame (these have a separate cap type as there is no room to
-- store their size)
block small_frame_cap {
    field capFMappedASIDLow  10
    field capFVMRights       2
    field_high capFMappedAddress 20

    field capFIsDevice       1
#ifdef CONFIG_TK1_SMMU
    field capFIsIOSpace      1
    field capFMappedASIDHigh 6
#else
    field capFMappedASIDHigh 7
#endif
    field_high capFBasePtr  20
    field capType            4
}

-- 64k, 1M, 16M frames
block frame_cap {
    field capFSize           2
    field capFMappedASIDLow  10
    field capFVMRights       2
    field_high capFMappedAddress 18

    padding                  2
    field capFIsDevice       1
    field capFMappedASIDHigh 7
    field_high capFBasePtr  18
    field capType            4
}

-- Second-level page table
block page_table_cap {
    padding                   2
    field capPTIsMapped       1
    field capPTMappedASID    17
#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
    field_high capPTMappedAddress 12
#else
    padding                   1
    field_high capPTMappedAddress 11
#endif

    field_high capPTBasePtr  22
    padding                   6
    field capType             4
}

-- First-level page table (page directory)
block page_directory_cap(capPDMappedASID, capPDIsMapped,
                         capPDBasePtr, capType) {
    padding                 15
    field capPDMappedASID   17

    field_high capPDBasePtr 18
    padding                  9
    field capPDIsMapped      1
    field capType            4
}

-- Cap to the table of 2^8 ASID pools
block asid_control_cap {
    padding       32

    padding       28
    field capType  4
}

-- Cap to a pool of 2^10 asids
block asid_pool_cap {
    padding                15
    field capASIDBase      17

    field_high capASIDPool 28
    field capType          4
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block vcpu_cap {
    padding               32

    field_high capVCPUPtr 24
    field capType         8
}
#endif

#if CONFIG_MAX_NUM_NODES == 1

block sgi_signal_cap {
    field capSGIIRQ         16
    -- Do we want to support multiple targets
    -- with a single seL4_Send?
    field capSGITargetMask  16


    padding        24
    field capType  8
}

#endif


#ifdef CONFIG_TK1_SMMU
-- IO space caps
-- each module has an engine that can be enabled
-- the clients use the same module can be separately enabled
block io_space_cap {
    field   capModuleID       16
    field   capClientID       16

    padding                   24
    field   capType           8
}

block io_space_capdata {
    padding                 32

    field   moduleID        16
    field   clientID        16
}

block io_page_table_cap (capType, capIOPTIsMapped, capIOPTASID, capIOPTBasePtr, capIOPTMappedAddress) {
    field_high  capIOPTBasePtr          20
    padding                             12

    field_high  capIOPTMappedAddress    10
    padding                             6
    field       capIOPTASID             7
    field       capIOPTIsMapped         1
    field       capType                 8
}
#endif

-- NB: odd numbers are arch caps (see isArchCap())
tagged_union cap capType {
    mask 4 0xe
    mask 8 0x0e

    -- 4-bit tag caps
    tag null_cap             0
    tag untyped_cap          2
    tag endpoint_cap         4
    tag notification_cap     6
    tag reply_cap            8
    tag cnode_cap           10
    tag thread_cap          12
    -- Do not extend even 4-bit caps types beyond 12, as we use
    -- 14 (0xe) to determine which caps are 8-bit.

    -- 4-bit tag arch caps
    tag small_frame_cap      1
    tag frame_cap            3
    tag asid_pool_cap        5
    tag page_table_cap       7
    tag page_directory_cap   9
    tag asid_control_cap    11
    -- Do not extend odd 4-bit caps types beyond 13, as we use
    -- 15 (0xf) to determine which caps are 8-bit.

    -- 8-bit tag caps
    tag irq_control_cap     0x0e
    tag irq_handler_cap     0x1e
    tag zombie_cap          0x2e
    tag domain_cap          0x3e
#ifdef CONFIG_KERNEL_MCS
    tag sched_context_cap   0x4e
    tag sched_control_cap   0x5e
#endif

    -- 8-bit tag arch caps
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    tag vcpu_cap            0x0f
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

    -- we use the same names as for x86 IOMMU caps
#ifdef CONFIG_TK1_SMMU
    tag io_space_cap            0x1f
    tag io_page_table_cap       0x2f
#endif
#if CONFIG_MAX_NUM_NODES == 1
    tag sgi_signal_cap          0x3f
#endif
}

---- Arm specific fault types

block VMFault {
    field address 32
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    field FSR 26
    padding 1
    field instructionFault 1
#else
    field FSR 14
    field instructionFault 1
    padding 13
#endif
    field seL4_FaultType 4
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block VGICMaintenance {
    field idx        6
    field idxValid   1
    padding         25
    padding         28
    field seL4_FaultType  4
}

block VCPUFault {
    field hsr       32
    padding         28
    field seL4_FaultType  4
}

block VPPIEvent {
    field irq_w     10
    padding         50
    field seL4_FaultType  4
}
#endif

---- ARM-specific object types

block stored_hw_asid {
    field asid 8
    field valid 1
    padding 21
    field pdeType 2
}

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
-- Short descriptors

-- Page directory entries
block pde_invalid {
    field stored_hw_asid 8
    field stored_asid_valid 1
    padding 21
    field pdeType 2
}

block pde_coarse {
    field_high address 22
    field P 1
    field Domain 4
    padding 3
    field pdeType 2
}

block pde_section {
    field_high address 12
    padding 1
    field size 1
    field nG 1
    field S 1
    field APX 1
    field TEX 3
    field AP 2
    field P 1
    field Domain 4
    field XN 1
    field C 1
    field B 1
    field pdeType 2
}

block pde_reserved {
    padding 30
    field pdeType 2
}

tagged_union pde pdeType {
    tag pde_invalid 0
    tag pde_coarse 1
    tag pde_section 2
    tag pde_reserved 3
}

-- Page table entries
block pte_large {
    field_high address 16
    field XN 1
    field TEX 3
    field nG 1
    field S 1
    field APX 1
    padding 3
    field AP 2
    field C 1
    field B 1
    field pteSize 1
    field reserved 1 -- must be set
}

block pte_small {
    field_high address 20
    field nG 1
    field S 1
    field APX 1
    field TEX 3
    field AP 2
    field C 1
    field B 1
    field pteSize 1
    field XN 1
}

tagged_union pte pteSize {
    tag pte_large 0
    tag pte_small 1
}

#else /* CONFIG_ARM_HYPERVISOR_SUPPORT */
-- These #defines allow for writing the bitfields here such
-- that they match the names in the ARM hardware, yet can
-- be used transparently by the existing C code that was
-- written for the short descriptors
#define pdeS2         pde
#define pdeS2Type     pdeType
#define pdeS2_section pde_section
#define pdeS2_coarse  pde_coarse
#define pdeS2_invalid pde_invalid

#define pteS2         pte
#define pteS2Type     pteType
#define pteS2_small   pte_small
#define pteS2_invalid pte_invalid

-- Stage 2 Long descriptors
-- Page directory entries

block pdeS2_invalid {
    padding 32
    field stored_hw_asid 8
    field stored_asid_valid 1
    padding 21
    field pdeS2Type 2
}

block pdeS2_section {
    padding 9
    field XN 1
    padding 1
    field contiguous_hint 1
    padding 12
    padding 8
    field_high address 20
    padding 1
    field AF 1
    field SH 2
    field HAP 2
    field MemAttr 4
    field pdeS2Type 2
}

block pdeS2_coarse {
    padding 24
    padding 8
    field_high address 20
    padding 10
    field pdeS2Type 2
}

tagged_union pdeS2 pdeS2Type {
    tag pdeS2_invalid  0
    tag pdeS2_section  1
    tag pdeS2_coarse   3
}

-- Page table entries
block pteS2_invalid {
    padding 62
    field pteS2Type 2
}

block pteS2_small {
    padding 9
    field XN 1
    padding 1
    field contiguous_hint 1
    padding 12
    padding 8
    field_high address 20
    padding 1
    field AF 1
    field SH 2
    field HAP 2
    field MemAttr 4
    field pteS2Type 2
}

tagged_union pteS2 pteS2Type {
    tag pteS2_invalid  0
    tag pteS2_small    3
}

-- Stage 1
-- Page directory entries
block pdeS1_invalid {
    padding 62
    field pdeS1Type 2
}

block pdeS1_section {
    padding 9
    field XN 1
    field PXN 1
    field contiguous_hint 1
    padding 12
    padding 8
    field_high address 20
    field nG 1
    field AF 1
    field SH 2
    field AP 2
    field NS 1
    field AttrIndx 3
    field pdeS1Type 2
}

block pdeS1_coarse {
    field NSTable 1
    field APTable 2
    field XNTable 1
    field PXNTable 1
    padding 19
    padding 8
    field_high address 20
    padding 10
    field pdeS1Type 2
}

tagged_union pdeS1 pdeS1Type {
    tag pdeS1_invalid  0
    tag pdeS1_section  1
    tag pdeS1_coarse   3
}

-- Page table entries
block pteS1_invalid {
    padding 62
    field pteS1Type 2
}

block pteS1_small {
    padding 9
    field XN 1
    field PXN 1
    field contiguous_hint 1
    padding 12
    padding 8
    field_high address 20
    field nG 1
    field AF 1
    field SH 2
    field AP 2
    field NS 1
    field AttrIndx 3
    field pteS1Type 2
}

tagged_union pteS1 pteS1Type {
    tag pteS1_invalid  0
    tag pteS1_small    3
}

#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

-- VM attributes

block vm_attributes {
    padding 29
    field armExecuteNever  1
    field armParityEnabled 1
    field armPageCacheable 1
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block virq_invalid {
    padding             2
    field virqType      2
    padding             8
    field virqEOIIRQEN  1
    padding             19
}

block virq_active {
    padding             2
    field virqType      2
    padding             8
    field virqEOIIRQEN  1
    padding             19
}

block virq_pending {
    padding             1
    field virqGroup     1
    field virqType      2
    field virqPriority  5
    padding             3
    field virqEOIIRQEN  1
    padding             9
    field virqIRQ       10
}

tagged_union virq virqType {
    tag virq_invalid    0
    tag virq_pending    1
    tag virq_active     2
}
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#ifdef CONFIG_HARDWARE_DEBUG_API
-- ARM breakpoint/watchpoint register layouts

-- Debug ID reg
block dbg_didr {
    field numWrps 4
    field numBrps 4
    field numContextIdBrps 4
    field version 4
    padding 8
    field variant 4
    field revision 4
}

-- Status and control reg
block dbg_dscr {
    padding 3
    padding 10
    field nonSecureState 1
    field securePrivilegedNIDebugDisabled 1
    field securePrivilegedIDebugDisabled 1
    field monitorDebugEnable 1
    field haltingDebugEnable 1
    padding 1
    field disableAllUserAccesses 1
    field interruptDisable 1
    field debugAcknowledge 1
    padding 2
    field stickyImpreciseAbort 1
    field stickyPreciseAbort 1
    field methodOfEntry 4
    padding 2
}

-- Breakpoint control reg
block dbg_bcr {
    padding 3
    field addressMask 5
    field breakpointType 4
    field linkedBrp 4
    field secureStateControl 2
    field hypeModeControl 1
    padding 4
    field byteAddressSelect 4
    padding 2
    field supervisorAccess 2
    field enabled 1
}

-- Watchpoint control reg
block dbg_wcr {
    padding 3
    field addressMask 5
    padding 3
    field enableLinking 1
    field linkedBrp 4
    field secureStateControl 2
    field hypeModeControl 1
    field byteAddressSelect 8
    field loadStore 2
    field supervisorAccess 2
    field enabled 1
}
#endif /* CONFIG_HARDWARE_DEBUG_API */

#include <sel4/arch/shared_types.bf>
