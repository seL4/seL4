--
-- Copyright 2019, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>
-- Default base size: uint32_t
base 32

block virq_invalid {
    padding             2
    field virqType      2
    padding             8
    field virqEOIIRQEN  1
    padding             19
}

block virq_active {
    padding             2
    field virqType      2
    padding             8
    field virqEOIIRQEN  1
    padding             19
}

block virq_pending {
    padding             1
    field virqGroup     1
    field virqType      2
    field virqPriority  5
    padding             3
    field virqEOIIRQEN  1
    padding             9
    field virqIRQ       10
}

tagged_union virq virqType {
    tag virq_invalid    0
    tag virq_pending    1
    tag virq_active     2
}
