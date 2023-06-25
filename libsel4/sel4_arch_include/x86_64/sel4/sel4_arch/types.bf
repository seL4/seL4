--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

#include <sel4/config.h>

base 64

block VMFault {
   padding 960
   field IP 64
   field Addr 64
   field PrefetchFault 64
   field FSR 64
   padding 60
   field seL4_FaultType 4
}

block NullFault {
   padding 1216
   padding 60
   field seL4_FaultType 4
}

block CapFault {
   padding 768
   field IP   64
   field Addr 64
   field InRecvPhase 64
   field LookupFailureType 64
   -- these vary according to LookupFailureType
   field MR4 64
   field MR5 64
   field MR6 64
   padding 60
   field seL4_FaultType 4
}

block UnknownSyscall {
   field RAX 64
   field RBX 64
   field RCX 64
   field RDX 64
   field RSI 64
   field RDI 64
   field RBP 64
   field R8 64
   field R9 64
   field R10 64
   field R11 64
   field R12 64
   field R13 64
   field R14 64
   field R15 64
   field FaultIP 64
   field RSP 64
   field FLAGS 64
   field Syscall 64
   padding 60
   field seL4_FaultType 4
}

block UserException {
   padding 896
   field FaultIP 64
   field Stack   64
   field FLAGS  64
   field Number  64
   field Code    64
   padding 60
   field seL4_FaultType 4
}

#ifdef CONFIG_HARDWARE_DEBUG_API
block DebugException {
    padding 960
    field FaultIP 64
    field ExceptionReason 64
    field TriggerAddress 64
    field BreakpointNumber 64
    padding 60
    field seL4_FaultType 4
}
#endif

#ifdef CONFIG_KERNEL_MCS
block Timeout {
    padding 1088
    field data 64
    field consumed 64
    padding 60
    field seL4_FaultType 4
}
#endif

#include <sel4/arch/shared_types.bf>
