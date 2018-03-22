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

base 64

block VMFault {
    padding 576
    field IP 64
    field Addr 64
    field PrefetchFault 64
    field FSR 64
    padding 61
    field seL4_FaultType 3
}

block NullFault {
    padding 832
    padding 61
    field seL4_FaultType 3
}

block CapFault {
    padding 384
    field IP   64
    field Addr 64
    field InRecvPhase 64
    field LookupFailureType 64
    -- these vary according to LookupFailureType
    field MR4 64
    field MR5 64
    field MR6 64
    padding 61
    field seL4_FaultType 3
}

block UnknownSyscall {
    field X0 64
    field X1 64
    field X2 64
    field X3 64
    field X4 64
    field X5 64
    field X6 64
    field X7 64
    field FaultIP 64
    field SP 64
    field LR 64
    field SPSR 64
    field Syscall 64
    padding 61
    field seL4_FaultType 3
}

block UserException {
    padding 512
    field FaultIP 64
    field Stack   64
    field SPSR  64
    field Number  64
    field Code    64
    padding 61
    field seL4_FaultType 3
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
block VGICMaintenance {
    padding 768
    field IDX 64
    padding 61
    field seL4_FaultType 3
}

block VCPUFault {
    padding 768
    padding 32
    field HSR 32
    padding 61
    field seL4_FaultType 3
}
#endif

#ifdef CONFIG_HARDWARE_DEBUG_API
block DebugException {
    padding 576
    field FaultIP 64
    field ExceptionReason 64
    field TriggerAddress 64
    field BreakpointNumber 64
    padding 61
    field seL4_FaultType 3
}
#endif

#include <sel4/arch/shared_types.bf>
