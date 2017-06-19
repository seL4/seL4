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

#include <autoconf.h>

base 32

block VMFault {
   padding 288
   field IP 32
   field Addr 32
   field PrefetchFault 32
   field FSR 32
   padding 29
   field seL4_FaultType 3
}

block NullFault {
   padding 416
   padding 29
   field seL4_FaultType 3
}

block CapFault {
   padding 192
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
   field R0 32
   field R1 32
   field R2 32
   field R3 32
   field R4 32
   field R5 32
   field R6 32
   field R7 32
   field FaultIP 32
   field SP 32
   field LR 32
   field CPSR 32
   field Syscall 32
   padding 29
   field seL4_FaultType 3
}

block UserException {
   padding 256
   field FaultIP 32
   field Stack   32
   field CPSR    32
   field Number  32
   field Code    32
   padding 29
   field seL4_FaultType 3
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block VGICMaintenance {
    padding 384
    field IDX 32
    padding 29
    field seL4_FaultType 3
}

block VCPUFault {
    padding 384
    field HSR 32
    padding 29
    field seL4_FaultType 3
}
#endif

#ifdef CONFIG_HARDWARE_DEBUG_API
block DebugException {
    padding 288
    field FaultIP 32
    field ExceptionReason 32
    field TriggerAddress 32
    field BreakpointNumber 32
    padding 29
    field seL4_FaultType 3
}
#endif

#include <sel4/arch/shared_types.bf>
