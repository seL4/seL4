--
-- Copyright 2017, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230.
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(DATA61_BSD)
--

-- this file contains types shared between libsel4 and the kernel

base 64

block seL4_MessageInfo {
    field label 52
    field capsUnwrapped 3
    field extraCaps 2
    field length 7
}

block seL4_CapRights {
    padding 32

    padding 29
    field capAllowGrant 1
    field capAllowRead 1
    field capAllowWrite 1
}

-- CNode cap data
block seL4_CNode_CapData {
    field guard 58
    field guardSize 6
}
