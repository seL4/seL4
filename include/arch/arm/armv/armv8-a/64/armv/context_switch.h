/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARMV_CONTEXT_SWITCH_H__
#define __ARMV_CONTEXT_SWITCH_H__

#include <config.h>
#include <arch/kernel/vspace.h>

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

#endif /* __ARMV_CONTEXT_SWITCH_H__ */
