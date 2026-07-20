--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

-- this file contains sel4arch specific types shared between libsel4 and the kernel

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
    tag Timeout       5

    -- arch specific faults
    tag VMFault 6
#else
    -- arch specific faults
    tag VMFault 5
#endif
}

#ifdef CONFIG_IOMMU

block seL4_X86_IOSpace_CapData {
#ifdef CONFIG_ARCH_X86_64
    padding 32
#endif
    field domainID              16
    field pciBus                8
    field pciDev                5
    field pciFun                3
}

#endif
