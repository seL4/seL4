--
-- Copyright 2014, General Dynamics C4 Systems
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>

base 32

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
    padding     4
    field       rmrr                1
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
