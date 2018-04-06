--
-- Copyright 2018, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230.
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(DATA61_GPL)
--

--
-- Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
-- Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>

#include <config.h>

---- Default base size: uint32_t
base 32(32,1)

-- Including the common structures.bf is neccessary because
-- we need the structures to be visible here when building
-- the capType
#include <object/structures_32.bf>

-- frames
block frame_cap {
    field       capFMappedASID      9
    field_high  capFBasePtr         20

    padding                         5

    field       capFSize            2
    field       capFVMRights        3
    field       capFIsDevice        1
    field_high  capFMappedAddress   20
    field       capType             4
}

-- N-level page table
block page_table_cap {
    field       capPTMappedASID     9
    field_high  capPTBasePtr        20

    padding                         10
    field       capPTIsMapped       1
    field_high  capPTMappedAddress  20
    field       capType             4
}

-- Cap to the table of 1 ASID pool
block asid_control_cap {
    padding             32
    padding             28
    field   capType     4
}

-- Cap to a pool of 2^9 ASIDs
block asid_pool_cap {
    padding                     23
    field       capASIDBase     9

    field_high  capASIDPool     28
    field       capType         4
}

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

    -- 4-bit tag arch caps
    tag frame_cap           1
    tag page_table_cap      3
    tag asid_control_cap    11
    tag asid_pool_cap       13

    -- 8-bit tag caps
    tag irq_control_cap     0x0e
    tag irq_handler_cap     0x1e
    tag zombie_cap          0x2e
    tag domain_cap          0x3e
}

---- Arch-independent object types

block VMFault {
    field     address           32

    field     FSR               5
    padding                     7
    field     instructionFault  1
    padding                     16
    field     seL4_FaultType    3
}

-- VM attributes

block vm_attributes {
    padding 31
    field riscvExecuteNever  1
}

---- RISCV-specific object types

-- RISC-V PTE format (priv-1.10) requires MSBs after PPN to be reserved 0s
block pte {
    field ppn              22
    field sw               2
    field dirty            1
    field accessed         1
    field global           1
    field user             1
    field execute          1
    field write            1
    field read             1
    field valid            1
}

-- RISC-V SATP (priv-1.10) Supervisor Address Translation and Protection
-- This register was originally named sptbr and some toolchains still use
-- sptbr when it refers to satp.

block satp {
    field mode          1
    field asid          9
    field ppn           22
}
#include <arch/api/shared_types.bf>
