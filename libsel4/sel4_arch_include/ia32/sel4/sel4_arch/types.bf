--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

#include <sel4/config.h>

base 32

block VMFault {
   padding 224
   field IP 32
   field Addr 32
   field PrefetchFault 32
   field FSR 32
   padding 28
   field seL4_FaultType 4
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
   field EAX 32
   field EBX 32
   field ECX 32
   field EDX 32
   field ESI 32
   field EDI 32
   field EBP 32
   field FaultIP 32
   field ESP 32
   field EFLAGS 32
   field Syscall 32
   padding 28
   field seL4_FaultType 4
}

block UserException {
   padding 192
   field FaultIP 32
   field Stack   32
   field EFLAGS  32
   field Number  32
   field Code    32
   padding 28
   field seL4_FaultType 4
}

#ifdef CONFIG_HARDWARE_DEBUG_API
block DebugException {
    padding 224
    field FaultIP 32
    field ExceptionReason 32
    field TriggerAddress 32
    field BreakpointNumber 32
    padding 28
    field seL4_FaultType 4
}
#endif

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
