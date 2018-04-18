--
-- Copyright 2016, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(DATA61_BSD)
--

--
-- Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
-- Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>

#include <autoconf.h>

base 32

---- Arch-independent object types

block VMFault {
    padding   256
    field     IP                32
    field     Addr              32
    field     PrefetchFault     32
    field     FSR               5
    padding                     8
    padding                     16
    field     seL4_FaultType    3
}

-- VM attributes
block vm_attributes {
    padding 31
    field riscvExecuteNever  1
}

block NullFault {
   padding 352
   padding 29
   field seL4_FaultType 3
}

block CapFault {
   padding 128
   field IP   32
   field Addr 32
   field InRecvPhase 32
   field LookupFailureType 32
   -- these vary according to LookupFailureType
   field MR4 32
   field MR5 32
   field MR6 32
   padding 29
   field seL4_FaultType 3
}

block UnknownSyscall {
   field FaultIP 32
   field SP 32
   field RA 32
   field A0 32
   field A1 32
   field A2 32
   field A3 32
   field A4 32
   field A5 32
   field A6 32
   field Syscall 32
   padding 29
   field seL4_FaultType 3
}

block UserException {
   padding 192
   field FaultIP 32
   field SP      32
   field FLAGS   32
   field Number  32
   field Code    32
   padding 29
   field seL4_FaultType 3
}

#include <sel4/arch/shared_types.bf>
