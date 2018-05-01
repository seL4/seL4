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

#ifndef __ARCH_MACHINE_TLB_H
#define __ARCH_MACHINE_TLB_H

#include <mode/machine.h>
#include <arch/smp/ipi_inline.h>
#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined(CONFIG_ARCH_AARCH64)
#include <armv/tlb.h>
#endif

static inline void invalidateTranslationSingleLocal(vptr_t vptr)
{
#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined(CONFIG_ARCH_AARCH64)
    invalidateLocalTLB_IPA_VMID(vptr);
#else
    invalidateLocalTLB_VAASID(vptr);
#endif
}

static inline void invalidateTranslationASIDLocal(hw_asid_t hw_asid)
{
#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) && defined(CONFIG_ARCH_AARCH64)
    invalidateLocalTLB_VMID(hw_asid);
#else
    invalidateLocalTLB_ASID(hw_asid);
#endif
}

static inline void invalidateTranslationAllLocal(void)
{
    invalidateLocalTLB();
}

static inline void invalidateTranslationSingle(vptr_t vptr)
{
    invalidateTranslationSingleLocal(vptr);
    SMP_COND_STATEMENT(doRemoteInvalidateTranslationSingle(vptr, MASK(CONFIG_MAX_NUM_NODES)));
}

static inline void invalidateTranslationASID(hw_asid_t hw_asid)
{
    invalidateTranslationASIDLocal(hw_asid);
    SMP_COND_STATEMENT(doRemoteInvalidateTranslationASID(hw_asid, MASK(CONFIG_MAX_NUM_NODES)));
}

static inline void invalidateTranslationAll(void)
{
    invalidateTranslationAllLocal();
    SMP_COND_STATEMENT(doRemoteInvalidateTranslationAll(MASK(CONFIG_MAX_NUM_NODES)));
}

#endif /* __ARCH_MACHINE_TLB_H */
