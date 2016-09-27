--
-- Copyright 2016, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230.
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(D61_BSD)
--

base 64

block Guard {
    padding 32

    field CapDataType 1
    padding 5
    field GuardBits 18
    field GuardSize 6
    padding 2
}

block Badge {
    padding 32

    field CapDataType 1
    padding 3
    field Badge 28
}

-- The ordering of these tags is important. The padding bits in the guard
-- Can be set and will be ignored by the kernel, but the padding bits in Badge
-- must be 0
tagged_union seL4_CapData CapDataType {
    tag Badge 0
    tag Guard 1
}
