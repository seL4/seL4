--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

-- this file contains types shared between libsel4 and the kernel 

base 32

block seL4_MessageInfo {
    field label 20
    field capsUnwrapped 3
    field extraCaps 2
    field length 7
}
