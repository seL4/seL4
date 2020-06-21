--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
-- Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>

---- Default base size: uint64_t
#if (CONFIG_PT_LEVELS == 3)
base 64(39,1)
#define BF_CANONICAL_RANGE 39
#else
#error "Only PT_LEVELS == 3 is currently supported on RISCV64"
#endif

-- Including the common structures.bf is neccessary because
-- we need the structures to be visible here when building
-- the capType
#include <object/structures_64.bf>

-- frames
block frame_cap {
    field       capFMappedASID      16
    field_high  capFBasePtr         39
    padding                         9

    field       capType             5
    field       capFSize            2
    field       capFVMRights        2
    field       capFIsDevice        1
    padding                         15
    field_high  capFMappedAddress   39
}

-- N-level page table
block page_table_cap {
    field       capPTMappedASID     16
    field_high  capPTBasePtr        39
    padding                         9

    field       capType             5
    padding                         19
    field       capPTIsMapped       1
    field_high  capPTMappedAddress  39
}

-- Cap to the table of 2^6 ASID pools
block asid_control_cap {
    padding 64

    field   capType     5
    padding             59
}

-- Cap to a pool of 2^10 ASIDs
block asid_pool_cap {
    padding 64

    field       capType         5
    field       capASIDBase     16
    padding                     6
    field_high  capASIDPool     37
}

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
    tag asid_control_cap    11
    tag asid_pool_cap       13
}

---- Arch-independent object types

block VMFault {
    field     address           64

    padding                     32
    field     FSR               5
    padding                     7
    field     instructionFault  1
    padding                     15
    field     seL4_FaultType    4
}

-- VM attributes

block vm_attributes {
    padding 32
    padding 31
    field riscvExecuteNever  1
}

---- RISCV-specific object types

-- RISC-V PTE format (priv-1.10) requires MSBs after PPN to be reserved 0s
-- RISC-V supports up to 56 bytes physical addressing.
-- Notice that the ppn field in the next two blocks is not field_high.
-- This means that ppn values are shifted manually in the code before the generated
-- bitfield accessors are used.
-- This is because Sv32 supports up to 34 bits of physical addressing and we
-- cannot return 34-bit values on RISCV-32.  This still affects us here in RISCV64
-- because the vspace source code is the same for both architectures and doing
-- the bit shifting manually only for 32-bit and not 64-bit is counter-intuitive.
block pte {
    padding                10
    field ppn              44
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
block satp {
    field mode          4
    field asid          16
    field ppn           44
}

#include <sel4/arch/shared_types.bf>
