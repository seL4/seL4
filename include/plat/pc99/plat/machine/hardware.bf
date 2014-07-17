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

base 32

-- PCI base address register (BAR)

block pci_bar_mem {
    field_high  base_address    28
    field       prefetchable    1
    field       above_4GB       1
    padding                     1
    field       pci_space       1
}

block pci_bar_io {
    field_high  base_address    30
    padding                     1
    field       pci_space       1
}

tagged_union pci_bar pci_space {
    tag pci_bar_mem 0
    tag pci_bar_io  1
}

#ifdef CONFIG_IOMMU
-- Intel VT-d Root Table Entry
block vtd_rte {
    padding                         96
    field_high  ctp                 20
    padding                         11
    field       present             1
}

-- Intel VT-d Context Entry
block vtd_cte {
    padding                         40
    field       did                 16
    --- Not using AVAIL and making it Reserved
    padding                         5
    field       aw                  3
    padding                         32
    field_high  asr                 20
    --- Assume ALH, EH as Reserved and FPD Enabled.
    padding                         8
    field       translation_type    2
    padding                         1
    field       present             1
}

-- Intel VT-d Page Table Entry
block vtd_pte {
    --- Assume AVAIL and TM as Reserved
    padding                         32

    field_high  addr                20
    padding                         10
    field       write               1
    field       read                1
}
#endif
