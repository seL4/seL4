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

void Arch_prepareSetDomain(tcb_t *tptr, dom_t dom)
{
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_flush_if_current(tptr);
    }
}

#ifdef ENABLE_SMP_SUPPORT
void Arch_prepareMigrateTCB(tcb_t *tptr)
{
    /* Flush VCPU before thread migration to avoid inconsistent state */
    if (config_set(CONFIG_ARM_HYPERVISOR_SUPPORT)) {
        vcpu_flush_if_current(tptr);
    }
}
#endif /* ENABLE_SMP_SUPPORT */
