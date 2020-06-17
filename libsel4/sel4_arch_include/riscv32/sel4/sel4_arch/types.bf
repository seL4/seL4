--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
-- Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
--
-- SPDX-License-Identifier: BSD-2-Clause
--

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
    padding                     15
    field     seL4_FaultType    4
}

-- VM attributes
block vm_attributes {
    padding 31
    field riscvExecuteNever  1
}

block NullFault {
   padding 352
   padding 28
   field seL4_FaultType 4
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
   padding 28
   field seL4_FaultType 4
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
   padding 28
   field seL4_FaultType 4
}

block UserException {
   padding 224
   field FaultIP 32
   field SP      32
   field Number  32
   field Code    32
   padding 28
   field seL4_FaultType 4
}

#ifdef CONFIG_KERNEL_MCS
block Timeout {
    padding 256
    field data 32
    field consumed_high 32
    field consumed_low 32
    padding 28
    field seL4_FaultType 4
}
#endif

#include <sel4/arch/shared_types.bf>
