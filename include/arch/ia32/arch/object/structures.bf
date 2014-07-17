--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

#include <config.h>

---- Default base size: uint32_t
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

block endpoint_cap {
    field capEPBadge 29
    field capCanGrant 1
    field capCanSend 1
    field capCanReceive 1

    field_high capEPPtr 28
    field capType 4
}

block async_endpoint_cap {
    field capAEPBadge 29
    padding 1
    field capAEPCanSend 1
    field capAEPCanReceive 1

    field_high capAEPPtr 28
    field capType 4
}

block reply_cap {
    padding 31
    field capReplyMaster 1

    field_high capTCBPtr 28
    field capType 4
}

block cnode_cap {
    padding 4
    field capCNodeRadix     5
    field capCNodeGuardSize 5
    field capCNodeGuard     18

    padding                 1
    field_high capCNodePtr  27
    field capType           4
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

---- IA32-specific cap types

block frame_cap {
    field       capFSize            1
#ifdef CONFIG_IOMMU
    field       capFIsIOSpace       1
#else
    padding                         1
#endif
    field       capFMappedASIDLow   10
    field_high  capFMappedAddress   20

    field       capFMappedASIDHigh  6
    field       capFVMRights        2
    field_high  capFBasePtr         20
    field       capType             4
}

-- Second-level page table
block page_table_cap {
    padding                         5
    field       capPTIsMapped       1
    field       capPTMappedASID     16
    field_high  capPTMappedAddress  10

    padding                         8
    field_high  capPTBasePtr        20
    field       capType             4
}

-- First-level page table (page directory)
block page_directory_cap {
    padding                         15
    field       capPDIsMapped       1
    field       capPDMappedASID     16

    padding                         8
    field_high  capPDBasePtr        20
    field       capType             4
}

-- Cap to the table of 2^6 ASID pools
block asid_control_cap {
    padding             32
    padding             28
    field   capType     4
}

-- Cap to a pool of 2^10 ASIDs
block asid_pool_cap {
    padding                     16
    field       capASIDBase     16

    padding                     8
    field_high  capASIDPool     20
    field       capType         4
}

-- IO Port Cap
block io_port_cap {
    field   capIOPortFirstPort 16
    field   capIOPortLastPort  16

    padding                    28
    field   capType            4
}

block io_port_capdata {
    field   firstPort          16
    field   lastPort           16
}

#ifdef CONFIG_IOMMU
-- IO Space Cap
block io_space_cap {
    field       capDomainID     16
    field       capPCIDevice    16

    padding                     28
    field       capType         4 
}

block io_space_capdata {
    field domainID              16
    field PCIDevice             16
}

-- IO Page Table Cap
block io_page_table_cap {
    padding                             2
    field       capIOPTIsMapped         1
    field       capIOPTLevel            2
    field_high  capIOPTMappedAddress    11
    field       capIOPTIOASID           16

    padding                     4
    field_high  capIOPTBasePtr  20
    field       capType         8
}
#endif

-- NB: odd numbers are arch caps (see isArchCap())
tagged_union cap capType {
    mask 4 0xe
    mask 8 0x0e

    -- 4-bit tag caps
    tag null_cap            0
    tag untyped_cap         2
    tag endpoint_cap        4
    tag async_endpoint_cap  6
    tag reply_cap           8
    tag cnode_cap           10
    tag thread_cap          12
    -- Do not extend even 4-bit caps types beyond 12, as we use 
    -- 14 (0xe) to determine which caps are 8-bit.

    -- 4-bit tag arch caps
    tag frame_cap           1
    tag page_table_cap      3
    tag page_directory_cap  5
    tag asid_control_cap    7
    tag asid_pool_cap       9  
    tag io_port_cap         11
#ifdef CONFIG_IOMMU
    tag io_space_cap        13
#endif /* CONFIG_IOMMU */
    -- Do not extend odd 4-bit caps types beyond 13, as we use 
    -- 15 (0xf) to determine which caps are 8-bit.
    
    -- 8-bit tag caps
    tag irq_control_cap     0x0e
    tag irq_handler_cap     0x1e
    tag zombie_cap          0x2e
    tag domain_cap	        0x3e

    -- 8-bit tag arch caps
#ifdef CONFIG_IOMMU
    tag io_page_table_cap   0x0f
#endif /* CONFIG_IOMMU */
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
    field     address           32
    field     FSR               5
    padding                     7
    field     instructionFault  1
    padding                     16
    field     faultType         3
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

tagged_union fault faultType {
    tag null_fault 0
    tag cap_fault 1
    tag vm_fault 2
    tag unknown_syscall 3
    tag user_exception 4
}

-- Thread state: size = 8 bytes
block thread_state {
    field blockingIPCBadge 29
    field blockingIPCCanGrant 1
    field blockingIPCIsCall 1
    field tcbQueued 1

    padding 31
    field blockingIPCDiminishCaps 1

    field_high blockingIPCEndpoint 28
    field tsType 4
}

-- VM attributes

block vm_attributes {
    padding 29
    field ia32PATBit 1
    field ia32PCDBit 1
    field ia32PWTBit 1
}

block ia32_pat_msr {
    padding     5
    field pa7   3
    padding     5
    field pa6   3
    padding     5
    field pa5   3
    padding     5
    field pa4   3
    padding     5
    field pa3   3
    padding     5
    field pa2   3
    padding     5
    field pa1   3
    padding     5
    field pa0   3
}

---- IA32 specific object types

-- GDT entries (Segment Desciptors)

block gdt_null {
    padding                         19
    field       desc_type           3
    padding                         42
}

block gdt_code {
    field       base_high           8
    field       granularity         1
    field       operation_size      1
    padding                         1
    field       avl                 1
    field       seg_limit_high      4
    field       present             1
    field       dpl                 2
    field       desc_type           3
    field       readable            1
    field       accessed            1
    field       base_mid            8
    field       base_low            16
    field       seg_limit_low       16
}

block gdt_data {
    field       base_high           8
    field       granularity         1
    field       operation_size      1
    padding                         1
    field       avl                 1
    field       seg_limit_high      4
    field       present             1
    field       dpl                 2
    field       desc_type           3
    field       writable            1
    field       accessed            1
    field       base_mid            8
    field       base_low            16
    field       seg_limit_low       16
}

block gdt_tss {
    field       base_high           8
    field       granularity         1
    padding                         2
    field       avl                 1
    field       limit_high          4
    field       present             1
    field       dpl                 2
    field       desc_type           3
    field       busy                1
    field       always_1            1
    field       base_mid            8
    field       base_low            16
    field       limit_low           16
}

tagged_union gdt_entry desc_type {
    tag gdt_null    0
    tag gdt_tss     2
    tag gdt_data    4
    tag gdt_code    6
}

-- IDT entries (Gate Desciptors)

block task_gate {
    padding                         16
    field       present             1
    field       dpl                 2
    padding                         2
    field       type                3
    padding                         8
    field       tss_seg_selector    16
    padding                         16
}

block interrupt_gate {
    field       offset_high         16
    field       present             1
    field       dpl                 2
    padding                         1
    field       gate_size           1
    field       type                3
    padding                         8
    field       seg_selector        16
    field       offset_low          16
}

block trap_gate {
    field       offset_high         16
    field       present             1
    field       dpl                 2
    padding                         1
    field       gate_size           1
    field       type                3
    padding                         8
    field       seg_selector        16
    field       offset_low          16
}

tagged_union idt_entry type {
    tag task_gate       5
    tag interrupt_gate  6
    tag trap_gate       7
}

-- Task State Segment (TSS)

block tss {
    field       io_map_base         16
    padding                         15
    field       trap                1
    padding                         16
    field       sel_ldt             16
    padding                         16
    field       gs                  16
    padding                         16
    field       fs                  16
    padding                         16
    field       ds                  16
    padding                         16
    field       ss                  16
    padding                         16
    field       cs                  16
    padding                         16
    field       es                  16
    field       edi                 32
    field       esi                 32
    field       ebp                 32
    field       esp                 32
    field       ebx                 32
    field       edx                 32
    field       ecx                 32
    field       eax                 32
    field       eflags              32
    field       eip                 32
    field       cr3                 32
    padding                         16
    field       ss2                 16
    field       esp2                32
    padding                         16
    field       ss1                 16
    field       esp1                32
    padding                         16
    field       ss0                 16
    field       esp0                32
    padding                         16
    field       prev_task           16
}

-- PDs and PTs

block pde_4k {
    field_high  pt_base_address     20
    field       avl                 3
    padding                         1
    field       page_size           1
    padding                         1
    field       accessed            1
    field       cache_disabled      1
    field       write_through       1
    field       super_user          1
    field       read_write          1
    field       present             1
}

block pde_4m {
    field_high  page_base_address   10
    padding                         9
    field       pat                 1
    field       avl                 3
    field       global              1
    field       page_size           1
    field       dirty               1
    field       accessed            1
    field       cache_disabled      1
    field       write_through       1
    field       super_user          1
    field       read_write          1
    field       present             1
}

tagged_union pde page_size {
    tag pde_4k 0
    tag pde_4m 1
}

block pte {
    field_high  page_base_address   20
    field       avl                 3
    field       global              1
    field       pat                 1
    field       dirty               1
    field       accessed            1
    field       cache_disabled      1
    field       write_through       1
    field       super_user          1
    field       read_write          1
    field       present             1
}

-- Local APIC

block apic_base_msr {
    field_high  base_addr           20
    field       enabled             1
    padding                         2
    field       is_bsp              1
    padding                         8
}

block apic_version {
    padding                         8
    field       max_lvt_entry       8
    padding                         8
    field       version             8
}

block apic_svr {
    padding                         22
    field       focus_processor_chk 1
    field       enabled             1
    field       spurious_vector     8
}

block apic_lvt {
    padding                         13
    field       timer_mode          2
    field       masked              1
    field       trigger_mode        1
    field       remote_irr          1
    field       pin_polarity        1
    field       delivery_status     1
    padding                         1
    field       delivery_mode       3
    field       vector              8
}

block apic_icr1 {
    padding                         12
    field       dest_shorthand      2
    padding                         2
    field       trigger_mode        1
    field       level               1
    padding                         1
    field       delivery_status     1
    field       dest_mode           1
    field       delivery_mode       3
    field       vector              8
}

block apic_icr2 {
    field       dest                8
    padding                         24
}
