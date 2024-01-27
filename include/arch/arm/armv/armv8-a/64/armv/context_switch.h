/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/kernel/vspace.h>


static inline void armv_contextSwitch_HWASID(vspace_root_t *vspace, asid_t asid)
{
    setCurrentUserVSpaceRoot(ttbr_new(asid, pptr_to_paddr(vspace)));
}

/*
 * In AARCH64, hardware and virtual asids are the same and are written
 * when updating the translation table base register.
 */
static inline void armv_contextSwitch(vspace_root_t *vspace, asid_t asid)
{
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    asid = getHWASID(asid);
#endif
    setCurrentUserVSpaceRoot(ttbr_new(asid, pptr_to_paddr(vspace)));
}

