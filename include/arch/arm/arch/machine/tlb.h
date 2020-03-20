/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

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

