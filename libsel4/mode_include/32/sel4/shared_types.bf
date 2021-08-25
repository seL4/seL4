--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

-- this file contains types shared between libsel4 and the kernel

base 32

block seL4_MessageInfo {
    field label 20
    field capsUnwrapped 3
    field extraCaps 2
    field length 7
}

-- Cap rights
block seL4_CapRights {
    padding 28
    field capAllowGrantReply 1
    field capAllowGrant 1
    field capAllowRead 1
    field capAllowWrite 1
}

-- CNode cap data
block seL4_CNode_CapData {
    padding 6
    field guard 18
    field guardSize 5
    padding 3
}
