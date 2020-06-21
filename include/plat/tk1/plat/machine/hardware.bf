--
-- Copyright 2016, General Dynamics C4 Systems
--
-- SPDX-License-Identifier: GPL-2.0-only
--

base 32

-- Tegra K1 System MMU page table structures
-- Note: the structures are not the ones defined in
-- the ARM system mmu specification, but SoC-specific
-- to the TK1 platform. This is because SoC vendors
-- tend to design their own SMMUs.
-- see 16.4.4.1 of the TK1 manual for details


-- IO PD entries
tagged_union iopde page_size {
    tag iopde_4m        0
    tag iopde_pt        1
}

block iopde_pt {
    field       read        1
    field       write       1
    field       nonsecure   1
    field       page_size   1
    padding                 8
    field_high  address     20
}

-- adding a 2-bit padding before address is because
-- we do not support addressing more than 4 GiB memory

block iopde_4m {
    field       read        1
    field       write       1
    field       nonsecure   1
    field       page_size   1
    padding                 6
    padding                 2
    field_high  address     10
    padding                 10
}


-- IO PT entries
block iopte {
    field       read        1
    field       write       1
    field       nonsecure   1
    padding                 7
    padding                 2
    field_high  address     20
}
