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
#ifdef CONFIG_KERNEL_MCS
    tag Timeout 5

    -- arch specific faults
    tag VMFault 6
#else
    -- arch specific faults
    tag VMFault 5
#endif
}

#ifdef CONFIG_HAVE_CHERI
-- Unpacked CHERI cap metadata passed to the kernel as an argument, from which the
-- kernel could construct an actual (compressed) CHERI capability, besides other
-- fields such as base, length, and address. The following format is just a
-- software definition and does not correspond to an architectural CHERI capability.
block CheriCapMeta {
#if __riscv_xlen == 64
    field AP      32
    padding       29
#else
    field AP      29
#endif
    field CT      1
    field M       1
    field V       1
}
#endif
