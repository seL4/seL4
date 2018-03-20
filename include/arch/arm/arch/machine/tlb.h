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

static inline void invalidateTranslationSingleLocal(vptr_t vptr)
{
    invalidateLocalTLB_VAASID(vptr);
}

static inline void invalidateTranslationASIDLocal(hw_asid_t hw_asid)
{
    invalidateLocalTLB_ASID(hw_asid);
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
