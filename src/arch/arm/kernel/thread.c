/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <kernel/thread.h>

void Arch_postModifyRegisters(tcb_t *tptr)
{
    /* Nothing to do */
}

void Arch_prepareNextDomain(void)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_flush();
    }
}
