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

-- Including the common structures.bf is neccessary because
-- we need the structures to be visible here when building
-- the capType
#include <object/structures.bf>

---- ARM-specific caps

-- 4k, 64k, 1M, 16M frames
block frame_cap {
    field capFMappedObjectHigh  20
    field capFMappedIndex       12

    field capFSize               3
    field capFVMRights           2
    padding                      1
    field capFMappedObjectLow    2
    field_high capFBasePtr      20
    field capType                4
}

-- Second-level page table
block page_table_cap {
    field_high capPTMappedObject    20
    field capPTMappedIndex          12

    field_high capPTBasePtr         22
    padding                          6
    field capType                    4
}

-- First-level page table (page directory)
block page_directory_cap {
    padding                 32

    field_high capPDBasePtr 18
    padding                 10
    field capType            4
}

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
    tag frame_cap            1 
    tag page_table_cap       3
    tag page_directory_cap   5
    -- Do not extend odd 4-bit caps types beyond 13, as we use 
    -- 15 (0xf) to determine which caps are 8-bit.
        
    -- 8-bit tag caps
    tag irq_control_cap     0x0e
    tag irq_handler_cap     0x1e
    tag zombie_cap          0x2e
    tag domain_cap          0x3e
}

---- Arch-independent object types

block vm_fault {
    field address 32
    field FSR 14
    field instructionFault 1
    padding 14
    field faultType 3
}

tagged_union fault faultType {
    tag null_fault 0
    tag cap_fault 1
    tag vm_fault 2
    tag unknown_syscall 3
    tag user_exception 4
}

---- ARM-specific object types

block stored_hw_asid {
    field asid 8
    field valid 1
    padding 21
    field pdeType 2
}

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

-- VM attributes

block vm_attributes {
    padding 29
    field armExecuteNever  1
    field armParityEnabled 1
    field armPageCacheable 1
}
