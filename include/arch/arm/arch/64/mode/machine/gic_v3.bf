--
-- Copyright 2019, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: GPL-2.0-only
--

#include <config.h>
-- Default base size: uint64_t
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
base 64(48,0)

block virq_inactive {
    field virqType      2
    field hw            1
    field group         1
    padding             4
    field priority      8
    padding             6
    field pintid        10
    field vintid        32
}

block virq_pending {
    field virqType      2
    field hw            1
    field group         1
    padding             4
    field priority      8
    padding             6
    field pintid        10
    field vintid        32
}

block virq_active {
    field virqType      2
    field hw            1
    field group         1
    padding             4
    field priority      8
    padding             6
    field pintid        10
    field vintid        32
}

block virq_pending_active {
    field virqType      2
    field hw            1
    field group         1
    padding             4
    field priority      8
    padding             6
    field pinitid       10
    field vintid        32
}

tagged_union virq virqType {
    tag virq_inactive       0
    tag virq_pending        1
    tag virq_active         2
    tag virq_pending_active 3
}
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
