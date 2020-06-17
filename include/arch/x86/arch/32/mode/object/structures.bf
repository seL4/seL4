--
-- Copyright 2014, General Dynamics C4 Systems
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>

---- Default base size: uint32_t
base 32

-- Including the common structures_32.bf is neccessary because
-- we need the structures to be visible here when building
-- the capType
#include <object/structures_32.bf>

---- IA32-specific cap types

block frame_cap {
    padding                         1
    field       capFSize            1
    field       capFMappedASIDLow   10
    field_high  capFMappedAddress   20

    padding                         1
    field       capFMapType         2
    field       capFIsDevice        1
    field       capFMappedASIDHigh  2
    field       capFVMRights        2
    field_high  capFBasePtr         20
    field       capType             4
}

-- Second-level page table
block page_table_cap {
    padding                         8
    field       capPTIsMapped       1
    field       capPTMappedASID     12
    field_high  capPTMappedAddress  11

    padding                         8
    field_high  capPTBasePtr        20
    field       capType             4
}

-- First-level page table (page directory)
block page_directory_cap {
    padding                         17
    field       capPDIsMapped       1
    field       capPDMappedASID     12
    field_high  capPDMappedAddress  2

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
    padding                     20
    field       capASIDBase     12

    padding                     8
    field_high  capASIDPool     20
    field       capType         4
}


-- IO Port Control Cap
block io_port_control_cap {
    padding 32

    padding 24
    field   capType             8
}

-- IO Port Cap
block io_port_cap {
    field   capIOPortFirstPort 16
    field   capIOPortLastPort  16

    padding                    8
#ifdef CONFIG_VTX
    field   capIOPortVPID      16
#else
    padding                    16
#endif
    field   capType            8
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
    field       capIOPTIsMapped         1
    field       capIOPTLevel            4
    field_high  capIOPTMappedAddress    11
    field       capIOPTIOASID           16

    padding                     4
    field_high  capIOPTBasePtr  20
    field       capType         8
}

#endif

#ifdef CONFIG_VTX

block vcpu_cap {
    padding                 32

    field_high capVCPUPtr   24
    field capType           8
}

-- Fourth-level EPT page table
block ept_pt_cap {
    field_high  capPTMappedAddress  11
    padding                         4
    field       capPTIsMapped       1
    field       capPTMappedASID     16

    field_high  capPTBasePtr        20
    padding                         4
    field       capType             8
}

-- third-level EPT page table (page directory)
block ept_pd_cap {
    field_high  capPDMappedAddress  3
    padding                         12
    field       capPDIsMapped       1
    field       capPDMappedASID     16

    field_high  capPDBasePtr        20
    padding                         4
    field       capType             8
}

-- Second-level EPT page table (page directory pointer table)
block ept_pdpt_cap {
    field_high  capPDPTMappedAddress 1
    padding                         14
    field       capPDPTIsMapped     1
    field       capPDPTMappedASID   16

    field_high  capPDPTBasePtr      20
    padding                         4
    field       capType             8
}

-- First-level EPT pml4
block ept_pml4_cap {
    padding                         15
    field       capPML4IsMapped     1
    field       capPML4MappedASID   16

    field_high  capPML4BasePtr      20
    padding                         4
    field       capType             8
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
    tag notification_cap    6
    tag reply_cap           8
    tag cnode_cap           10
    tag thread_cap          12
    -- Do not extend even 4-bit caps types beyond 12, as we use
    -- 14 (0xe) to determine which caps are 8-bit.

    -- 4-bit tag arch caps
    tag frame_cap           1
    tag page_table_cap      3
    tag page_directory_cap  5
    tag asid_control_cap    9
    tag asid_pool_cap       11
#ifdef CONFIG_IOMMU
    tag io_space_cap        13
#endif
    -- Do not extend odd 4-bit caps types beyond 13, as we use
    -- 15 (0xf) to determine which caps are 8-bit.

    -- 8-bit tag caps
    tag irq_control_cap     0x0e
    tag irq_handler_cap     0x1e
    tag zombie_cap          0x2e
    tag domain_cap	        0x3e
#ifdef CONFIG_KERNEL_MCS
    tag sched_context_cap   0x4e
    tag sched_control_cap   0x5e
#endif

    -- 8-bit tag arch caps
#ifdef CONFIG_IOMMU
    tag io_page_table_cap   0x0f
#endif
    tag io_port_cap         0x1f
#ifdef CONFIG_VTX
    tag vcpu_cap            0x2f
    tag ept_pt_cap          0x3f
    tag ept_pd_cap          0x4f
    tag ept_pdpt_cap        0x5f
    tag ept_pml4_cap        0x6f
#endif
    tag io_port_control_cap 0x7f
}

---- IA32 specific fault types

block VMFault {
    field     address           32
    field     FSR               5
    padding                     7
    field     instructionFault  1
    padding                     15
    field     seL4_FaultType    4
}

-- VM attributes

block vm_attributes {
    padding 29
    field x86PATBit 1
    field x86PCDBit 1
    field x86PWTBit 1
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

block pde_pt {
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

block pde_large {
    field_high  page_base_address   11
    padding                         8
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
    tag pde_pt 0
    tag pde_large 1
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

#ifdef CONFIG_VTX

block ept_pml4e {
    padding                         32
    field_high   pdpt_base_address  20
    padding                         9
    field        execute            1
    field        write              1
    field        read               1
}

block ept_pdpte {
    padding                         32
    field_high   pd_base_address    20
    field        avl_cte_depth      3
    padding                         6
    field        execute            1
    field        write              1
    field        read               1
}

block ept_pde_2m {
    padding                         32
    field_high   page_base_address  12
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
    padding                         32
    field_high   pt_base_address    20
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
    padding                         32
    field_high   page_base_address  20
    field        avl_cte_depth      2
    padding                         3
    field        ignore_pat         1
    field        type               3
    field        execute            1
    field        write              1
    field        read               1
}

block vmx_eptp {
    field_high paddr                20
    padding                         5
    field flags                     1
    field depth_minus_1             3
    field memory_type               3
}

#endif

block asid_map_none {
    padding                         30
    field type                      2
}

block asid_map_vspace {
    field_high vspace_root          20
    padding                         10
    field type                      2
}

#ifdef CONFIG_VTX
block asid_map_ept {
    field_high ept_root             20
    padding                         10
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

block cr3 {
    field_high  pd_base_address     20
    padding                         12
}

#include <sel4/arch/shared_types.bf>
