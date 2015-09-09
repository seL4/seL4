--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

base 32

block Guard {
    field CapDataType 1
    padding 5
    field GuardBits 18
    field GuardSize 5
    padding 3
}

block Badge {
    field CapDataType 1
    padding 3
    field Badge 28
}

# The ordering of these tags is important. The padding bits in the guard
# Can be set and will be ignored by the kernel, but the padding bits in Badge
# must be 0
tagged_union seL4_CapData CapDataType {
    tag Badge 0
    tag Guard 1
}

block seL4_MessageInfo {
    field label 20
    field capsUnwrapped 3
    field extraCaps 2
    field length 7
}
