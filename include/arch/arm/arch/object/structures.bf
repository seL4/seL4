--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

-- Default base size: uint32_t
base 32

---- Arch-independent caps
block null_cap {
    padding 32

    padding 28
    field capType 4
}

block untyped_cap {
    field capFreeIndex 27
    field capBlockSize 5

    field_high capPtr 28
    field capType     4
}

block endpoint_cap(capEPBadge, capCanGrant, capCanSend, capCanReceive,
                   capEPPtr, capType) {
    field_high capEPPtr 28
    padding 1
    field capCanGrant 1
    field capCanReceive 1
    field capCanSend 1

    field capEPBadge 28
    field capType 4
}

block async_endpoint_cap {
    field capAEPBadge 28
    padding 2
    field capAEPCanReceive 1
    field capAEPCanSend 1

    field_high capAEPPtr 28
    field capType 4
}

block reply_cap(capReplyMaster, capTCBPtr, capType) {
    padding 32

    field_high capTCBPtr 27
    field capReplyMaster 1
    field capType 4
}

-- This structure is modified on ARM for performance reasons. The user-visible
-- format of the data word is defined by cnode_capdata, below.
block cnode_cap(capCNodeRadix, capCNodeGuardSize, capCNodeGuard,
                capCNodePtr, capType) {
    padding 4
    field capCNodeGuardSize 5
    field capCNodeRadix 5
    field capCNodeGuard 18

    field_high capCNodePtr 27
    padding 1
    field capType 4
}

block cnode_capdata {
    padding 6
    field guard 18
    field guardSize 5
    padding 3
}

block thread_cap {
    padding              32

    field_high capTCBPtr 28
    field capType         4
}

block irq_control_cap {
    padding       32

    padding       24
    field capType  8
}

block irq_handler_cap {
    padding       24
    field capIRQ   8

    padding       24
    field capType  8
}

block zombie_cap {
    field capZombieID     32
    
    padding               18
    field capZombieType   6
    field capType         8
}

block domain_cap {
    padding 32

    padding 24
    field capType 8
}

---- ARM-specific caps

-- 4k frame (these have a separate cap type as there is no room to
-- store their size)
block small_frame_cap {
    field capFMappedASIDLow 10
    field capFVMRights       2
    field_high capFMappedAddress 20

    field capFMappedASIDHigh 8
    field_high capFBasePtr  20
    field capType            4
}

-- 64k, 1M, 16M frames
block frame_cap {
    field capFSize           2
    field capFMappedASIDLow 10
    field capFVMRights       2
    field_high capFMappedAddress 18

    padding                  2
    field capFMappedASIDHigh 8
    field_high capFBasePtr  18
    field capType            4
}

-- Second-level page table
block page_table_cap {
    padding                   1
    field capPTIsMapped       1
    field capPTMappedASID    18
#ifndef ARM_HYP
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
    padding                 14
    field capPDMappedASID   18

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
    padding                14
    field capASIDBase      18

    field_high capASIDPool 28
    field capType          4
}

#ifdef ARM_HYP
block vcpu_cap {
    padding               32

    field_high capVCPUPtr 24
    field capType         8
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
    tag async_endpoint_cap   6
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

    -- 8-bit tag arch caps
#ifdef ARM_HYP
    tag vcpu_cap                               0x0f
#endif /* ARM_HYP */
}

---- Arch-independent object types

-- Endpoint: size = 16 bytes
block endpoint {
    padding 64

    field_high epQueue_head 28
    padding 4

    field_high epQueue_tail 28
    padding 2
    field state 2
}

-- Async endpoint: size = 16 bytes
block async_endpoint {
    field aepData 32

    field aepMsgIdentifier 32

    field_high aepQueue_head 28
    padding 4

    field_high aepQueue_tail 28
    padding 2
    field state 2
}

-- Mapping database (MDB) node: size = 8 bytes
block mdb_node {
    field_high mdbNext 29
    padding 1
    field mdbRevocable 1
    field mdbFirstBadged 1

    field_high mdbPrev 29
    padding 3
}

-- Thread state data
--
-- tsType
-- * Running
-- * Restart
-- * Inactive
-- * BlockedOnReceive
--   - DiminishCaps
--   - Endpoint
-- * BlockedOnSend
--   - Endpoint
--   - CanGrant
--   - IsCall
--   - IPCBadge
--   - Fault
--     - faultType
--     * CapFault
--       - Address
--       - InReceivePhase
--       - LookupFailure
--         - lufType
--         * InvalidRoot
--         * MissingCapability
--           - BitsLeft
--         * DepthMismatch
--           - BitsFound
--           - BitsLeft
--         * GuardMismatch
--           - GuardFound
--           - BitsLeft
--           - GuardSize
--     * VMFault
--       - Address
--       - FSR
--       - FaultType
--     * UnknownSyscall
--       - Number
--     * UserException
--       - Number
--       - Code
-- * BlockedOnReply
-- * BlockedOnFault
--   - Fault
-- * BlockedOnAsyncEvent
--   - AEP
-- * Idle

-- Lookup fault: size = 8 bytes
block invalid_root {
    padding 62
    field lufType 2
}

block missing_capability {
    padding 56
    field bitsLeft 6
    field lufType 2
}

block depth_mismatch {
    padding 50
    field bitsFound 6
    field bitsLeft 6
    field lufType 2
}

block guard_mismatch {
    field guardFound 32
    padding 18
    field bitsLeft 6
    field bitsFound 6
    field lufType 2
}

tagged_union lookup_fault lufType {
    tag invalid_root 0
    tag missing_capability 1
    tag depth_mismatch 2
    tag guard_mismatch 3
}

-- Fault: size = 8 bytes
block null_fault {
    padding 61
    field faultType 3
}

block cap_fault {
    field address 32
    field inReceivePhase 1
    padding 28
    field faultType 3
}

block vm_fault {
    field address 32
#ifdef ARM_HYP
    field FSR 26
    padding 2
    field instructionFault 1
#else
    field FSR 14
    field instructionFault 1
    padding 14
#endif
    field faultType 3
}

block unknown_syscall {
    field syscallNumber 32
    padding 29
    field faultType 3
}

block user_exception {
    field number 32
    field code 29
    field faultType 3
}

#ifdef ARM_HYP
block vgic_maintenance {
    field idx        6
    field idxValid   1
    padding         25
    padding         29
    field faultType  3
}
#endif

tagged_union fault faultType {
    tag null_fault 0
    tag cap_fault 1
    tag vm_fault 2
    tag unknown_syscall 3
    tag user_exception 4
#ifdef ARM_HYP
    tag vgic_maintenance 5
#endif
}

-- Thread state: size = 8 bytes
block thread_state(blockingIPCBadge, blockingIPCCanGrant, blockingIPCIsCall,
                   tcbQueued, blockingIPCDiminishCaps, blockingIPCEndpoint,
                   tsType) {
    field blockingIPCBadge 28
    field blockingIPCCanGrant 1
    field blockingIPCIsCall 1
    padding 1
    field blockingIPCDiminishCaps 1
    
    -- this is fastpath-specific. it is useful to be able to write
    -- tsType and blockingIPCDiminishCaps without changing tcbQueued
    padding 31
    field tcbQueued 1

    field_high blockingIPCEndpoint 28
    field tsType 4
}

---- ARM-specific object types

block stored_hw_asid {
    field asid 8
    field valid 1
    padding 21
    field pdeType 2
}

#ifndef ARM_HYP
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

#else /* !ARM_HYP */
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

#endif /* !ARM_HYP */

-- VM attributes

block vm_attributes {
    padding 29
    field armExecuteNever  1
    field armParityEnabled 1
    field armPageCacheable 1
}
