--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>

---- Default base size: uint64_t
base 64(48,1)
#define BF_CANONICAL_RANGE 48

-- Including the common structures.bf is neccessary because
-- we need the structures to be visible here when building
-- the capType
#include <object/structures_64.bf>

---- x86-64 specific cap types

block frame_cap {
    -- The ASID doubles as the PCI bus/dev/fun when used in an IOMMU context
    -- so it is 16 instead of 12
    field       capFMappedASID      16
    field_high  capFBasePtr         48

    field       capType             5
    field       capFSize            2
    field       capFMapType         2
    field_high  capFMappedAddress   48
    field       capFVMRights        2
    field       capFIsDevice        1
    padding                         4
}

-- Second-level page table
block page_table_cap {
    padding                         4
    field       capPTMappedASID     12
    field_high  capPTBasePtr        48

    field       capType             5
    padding                         9
    field       capPTIsMapped       1
    field_high  capPTMappedAddress  28

    padding                         21
}

-- First-level page table (page directory)
block page_directory_cap {
    padding                         4
    field       capPDMappedASID     12
    field_high  capPDBasePtr        48

    field       capType             5
    padding                         9
    field       capPDIsMapped       1
    field_high  capPDMappedAddress  19

    padding                         30
}

block pdpt_cap {
    padding                          4
    field       capPDPTMappedASID    12
    field_high  capPDPTBasePtr       48

    field       capType              5
    field       capPDPTIsMapped      1
    field_high  capPDPTMappedAddress 10
    padding                          48
}

block pml4_cap(capPML4MappedASID, capPML4BasePtr, capType, capPML4IsMapped) {
    field       capPML4BasePtr      64

    field       capType             5
    field       capPML4IsMapped     1
    padding                         46
    field       capPML4MappedASID   12
}

-- Cap to the table of 2^6 ASID pools
block asid_control_cap {
    padding 64

    field   capType     5
    padding             59
}

-- Cap to a pool of 2^9 ASIDs
block asid_pool_cap {
    padding 64

    field       capType         5
    field       capASIDBase     12
    padding                     10
    field_high  capASIDPool     37
}

-- IO Port Control Cap
block io_port_control_cap {
    padding 64

    field   capType             5
    padding 59
}

-- IO Port Cap
block io_port_cap {
    padding 64
    field   capType            5
    padding 3
    field   capIOPortFirstPort 16
    field   capIOPortLastPort  16

    padding                    8
#ifdef CONFIG_VTX
    field   capIOPortVPID      16
#else
    padding                    16
#endif
}

#ifdef CONFIG_IOMMU

-- IO Space Cap
block io_space_cap {
    padding 64
    field       capType         5
    field       capDomainID     16
    field       capPCIDevice    16

    padding                     27
}

block io_space_capdata {
    padding 32
    field domainID              16
    field PCIDevice             16
}

-- IO Page Table Cap
block io_page_table_cap (capType, capIOPTIsMapped, capIOPTLevel, capIOPTMappedAddress, capIOPTIOASID, capIOPTBasePtr) {
    -- IOASID is actually a PCI bus/dev/fun and so is 16
    field       capIOPTIOASID           16
    field_high capIOPTBasePtr 48

    field       capType                 5
    padding                             6
    field       capIOPTIsMapped         1
    field       capIOPTLevel            4
    field_high  capIOPTMappedAddress    48
}

#endif

#ifdef CONFIG_VTX

block vcpu_cap {
    field capVCPUPtr                64

    field capType                   5
    padding                         59
}

-- Fourth-level EPT page table
block ept_pt_cap (capType, capPTMappedAddress, capPTIsMapped, capPTMappedASID, capPTBasePtr) {
    field       capPTBasePtr        64

    field       capType             5
    field       capPTIsMapped       1
    padding                         9
    field_high  capPTMappedAddress  28
    field       capPTMappedASID     16
    padding                         5
}

-- third-level EPT page table (page directory)
block ept_pd_cap (capType, capPDMappedAddress, capPDIsMapped, capPDMappedASID, capPDBasePtr) {
    field       capPDBasePtr        64

    field       capType             5
    field       capPDIsMapped       1
    padding                         9
    field_high  capPDMappedAddress  20
    field       capPDMappedASID     16
    padding                         13
}

-- Second-level EPT page table (page directory pointer table)
block ept_pdpt_cap (capType, capPDPTMappedAddress, capPDPTIsMapped, capPDPTMappedASID, capPDPTBasePtr) {
    field       capPDPTBasePtr      64

    field       capType             5
    field       capPDPTIsMapped     1
    padding                         9
    field_high  capPDPTMappedAddress 18
    field       capPDPTMappedASID   16
    padding                         15
}

-- First-level EPT pml4
block ept_pml4_cap (capType, capPML4IsMapped, capPML4MappedASID, capPML4BasePtr) {
    field       capPML4BasePtr      64

    field       capType             5
    field       capPML4IsMapped     1
    padding                         42

    field       capPML4MappedASID   16
}

#endif

-- NB: odd numbers are arch caps (see isArchCap())
tagged_union cap capType {
    -- 5-bit tag caps
    tag null_cap            0
    tag untyped_cap         2
    tag endpoint_cap        4
    tag notification_cap    6
    tag reply_cap           8
    tag cnode_cap           10
    tag thread_cap          12
    tag irq_control_cap     14
    tag irq_handler_cap     16
    tag zombie_cap          18
    tag domain_cap	        20
#ifdef CONFIG_KERNEL_MCS
    tag sched_context_cap   22
    tag sched_control_cap   24
#endif

    -- 5-bit tag arch caps
    tag frame_cap           1
    tag page_table_cap      3
    tag page_directory_cap  5
    tag pdpt_cap            7
    tag pml4_cap            9
    tag asid_control_cap    11
    tag asid_pool_cap       13
#ifdef CONFIG_IOMMU
    tag io_space_cap        15
    tag io_page_table_cap   17
#endif
    tag io_port_cap         19
#ifdef CONFIG_VTX
    tag vcpu_cap            21
    tag ept_pt_cap          23
    tag ept_pd_cap          25
    tag ept_pdpt_cap        27
    tag ept_pml4_cap        29
#endif
    tag io_port_control_cap 31
}

---- Arch-independent object types

block VMFault {
    field     address           64

    padding                     32
    field     FSR               5
    padding                     7
    field     instructionFault  1
    padding                     15
    field     seL4_FaultType         4
}

-- VM attributes

block vm_attributes {
    padding         61
    field x86PATBit 1
    field x86PCDBit 1
    field x86PWTBit 1
}

---- x86-64 specific object types

-- GDT entries (Segment Desciptors)

block gdt_null {
    padding                         19
    padding                         1
    field       seg_type            4
    padding                         40
}

block gdt_code {
    field       base_high           8
    field       granularity         1
    field       operation_size      1
    field       long_mode           1
    field       avl                 1
    field       seg_limit_high      4
    field       present             1
    field       dpl                 2
    field       always_1            1
    field       seg_type            4
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
    field       always_1            1
    field       seg_type            4
    field       base_mid            8
    field       base_low            16
    field       seg_limit_low       16
}

block gdt_tss {
    padding                         32
    field       base_63_32          32

    field       base_31_24          8
    field       granularity         1
    padding                         2
    field       avl                 1
    field       limit_high          4
    field       present             1
    field       dpl                 2
    padding                         1
    field       desc_type           4
    field       base_23_16          8
    field       base_15_0           16
    field       limit_low           16
}

tagged_union gdt_entry seg_type {
    tag gdt_null    0
    tag gdt_data    7
    tag gdt_code    11
}

-- IDT entries (Gate Desciptors)

block task_gate {
    padding                         32
    field       offset_63_32        32

    field       offset_31_16        16
    field       present             1
    field       dpl                 2
    padding                         1
    field       type                4
    padding                         5
    field       ist                 3
    field       tss_seg_selector    16
    field       offset_15_0         16
}

block interrupt_gate {
    padding                         32
    field       offset_63_32        32

    field       offset_31_16        16
    field       present             1
    field       dpl                 2
    padding                         1
    field       type                4
    padding                         5
    field       ist                 3
    field       seg_selector        16
    field       offset_15_0         16
}

block trap_gate {
    padding                         32
    field       offset_63_32        32

    field       offset_31_16        16
    field       present             1
    field       dpl                 2
    padding                         1
    field       type                4
    padding                         5
    field       ist                 3
    field       seg_selector        16
    field       offset_15_0         16
}

tagged_union idt_entry type {
    tag interrupt_gate  14
    tag trap_gate       15
}

-- Task State Segment (TSS)

block tss {
    field       io_map_base               16
    padding                               16
    padding                               32
    padding                               32
    field ist7_u                          32
    field ist7_l                          32
    field ist6_u                          32
    field ist6_l                          32
    field ist5_u                          32
    field ist5_l                          32
    field ist4_u                          32
    field ist4_l                          32
    field ist3_u                          32
    field ist3_l                          32
    field ist2_u                          32
    field ist2_l                          32
    field ist1_u                          32
    field ist1_l                          32
    padding                               32
    padding                               32
    field rsp2_u                          32
    field rsp2_l                          32
    field rsp1_u                          32
    field rsp1_l                          32
    field rsp0_u                          32
    field rsp0_l                          32
    padding                               32
}


block asid_map_none {
    padding                         62
    field type                      2
}

block asid_map_vspace {
    field_high vspace_root          48
    padding                         14
    field type                      2
}

#ifdef CONFIG_VTX
block asid_map_ept {
    field_high ept_root             48
    padding                         14
    field type                      2
}
#endif

tagged_union asid_map type {
    tag asid_map_none 0
    tag asid_map_vspace 1
#ifdef CONFIG_VTX
    tag asid_map_ept 2
#endif
}

-- PML4, PDPE, PDs and PTs, assuming 51-bit physical address
base 64(51,0)

block pml4e {
    field       xd                  1
    padding                         11
    padding                         1
    field_high  pdpt_base_address   39
    padding                         4
    padding                         1
    padding                         1
    field       accessed            1
    field       cache_disabled      1
    field       write_through       1
    field       super_user          1
    field       read_write          1
    field       present             1
}

block pdpte_1g {
    field       xd                  1
    padding                         11
    padding                         1
    field_high  page_base_address   21
    padding                         17
    field       pat                 1
    padding                         3
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

block pdpte_pd {
    field       xd                  1
    padding                         11
    padding                         1
    field_high  pd_base_address     39
    padding                         4
    field       page_size           1
    padding                         1
    field       accessed            1
    field       cache_disabled      1
    field       write_through       1
    field       super_user          1
    field       read_write          1
    field       present             1
}

tagged_union pdpte page_size {
    tag         pdpte_1g            1
    tag         pdpte_pd            0
}

block pde_large {
    field       xd                  1
    padding                         11
    padding                         1
    field_high  page_base_address   30
    padding                         8
    field       pat                 1
    padding                         3
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

block pde_pt {
    field       xd                  1
    padding                         11
    padding                         1
    field_high  pt_base_address     39
    padding                         4
    field       page_size           1
    padding                         1
    field       accessed            1
    field       cache_disabled      1
    field       write_through       1
    field       super_user          1
    field       read_write          1
    field       present             1
}

tagged_union pde page_size {
    tag pde_pt 0
    tag pde_large 1
}

block pte {
    field       xd                  1
    padding                         11
    padding                         1
    field_high  page_base_address   39
    padding                         3
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

#ifdef CONFIG_VTX

block ept_pml4e {
    padding                         13
    field_high   pdpt_base_address  39
    padding                         9
    field        execute            1
    field        write              1
    field        read               1
}

block ept_pdpte {
    padding                         13
    field_high   pd_base_address    39
    field        avl_cte_depth      3
    padding                         6
    field        execute            1
    field        write              1
    field        read               1
}

block ept_pde_2m {
    padding                         13
    field_high   page_base_address  31
    padding                         8
    field        avl_cte_depth      2
    padding                         2
    field        page_size          1
    field        ignore_pat         1
    field        type               3
    field        execute            1
    field        write              1
    field        read               1
}

block ept_pde_pt {
    padding                         13
    field_high   pt_base_address    39
    field        avl_cte_depth      3
    padding                         1
    field        page_size          1
    padding                         4
    field        execute            1
    field        write              1
    field        read               1
}

tagged_union ept_pde page_size {
    tag ept_pde_pt 0
    tag ept_pde_2m 1
}

block ept_pte {
    padding                         13
    field_high   page_base_address  39
    field        avl_cte_depth      2
    padding                         3
    field        ignore_pat         1
    field        type               3
    field        execute            1
    field        write              1
    field        read               1
}

block vmx_eptp {
    padding                         13
    field_high paddr                39
    padding                         5
    field flags                     1
    field depth_minus_1             3
    field memory_type               3
}

#endif

block cr3 {
    padding                         13
    field_high  pml4_base_address   39
    field       pcid                12
}


#include <sel4/arch/shared_types.bf>
