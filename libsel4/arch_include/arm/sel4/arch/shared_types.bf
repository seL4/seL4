--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

-- this file contains types shared between libsel4 and the kernel

tagged_union seL4_Fault seL4_FaultType {
    -- generic faults
    tag NullFault 0
    tag CapFault 1
    tag UnknownSyscall 2
    tag UserException 3
#ifdef CONFIG_HARDWARE_DEBUG_API
    tag DebugException 4
#endif
#ifdef CONFIG_KERNEL_MCS
    tag Timeout 5

    -- arch specific faults
    tag VMFault 6

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    tag VGICMaintenance 7
    tag VCPUFault 8
    tag VPPIEvent 9
#endif
#else
    -- arch specific faults
    tag VMFault 5

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    tag VGICMaintenance 6
    tag VCPUFault 7
    tag VPPIEvent 8
#endif
#endif

}
