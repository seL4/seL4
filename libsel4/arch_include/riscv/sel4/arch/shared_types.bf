--
-- Copyright 2018, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(DATA61_BSD)
--
-- this file contains sel4arch specific types shared between libsel4 and the kernel

tagged_union seL4_Fault seL4_FaultType {
    -- generic faults
    tag NullFault 0
    tag CapFault 1
    tag UnknownSyscall 2
    tag UserException 3
    -- arch specific faults
    tag VMFault 5
}
