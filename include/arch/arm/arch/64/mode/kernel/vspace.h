/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#define activate_global_pd activate_kernel_vspace
#define MODE_RESERVED 0

/* ==================== BOOT CODE FINISHES HERE ==================== */

bool_t CONST isVTableRoot(cap_t cap);
bool_t CONST isValidNativeRoot(cap_t cap);

pgde_t *pageUpperDirectoryMapped(asid_t asid, vptr_t vaddr, pude_t *pud);
pude_t *pageDirectoryMapped(asid_t asid, vptr_t vaddr, pde_t *pd);
void unmapPageUpperDirectory(asid_t asid, vptr_t vaddr, pude_t *pud);
void unmapPageDirectory(asid_t asid, vptr_t vaddr, pde_t *pd);

void unmapPageTable(asid_t asid, vptr_t vaddr, pte_t *pt);
void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, pptr_t pptr);

void deleteASIDPool(asid_t base, asid_pool_t *pool);
void deleteASID(asid_t asid, vspace_root_t *vspace);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
hw_asid_t getHWASID(asid_t asid);
#endif

#ifdef __clang__
static const region_t BOOT_RODATA mode_reserved_region[] = {};
#else
static const region_t BOOT_RODATA *mode_reserved_region = NULL;
#endif

#ifdef AARCH64_VSPACE_S2_START_L1

#define cap_vtable_root_cap cap_page_upper_directory_cap
#define cap_vtable_root_get_mappedASID(_c) \
    cap_page_upper_directory_cap_get_capPUDMappedASID(_c)
#define cap_vtable_root_get_basePtr(_c) \
    VSPACE_PTR(cap_page_upper_directory_cap_get_capPUDBasePtr(_c))
#define cap_vtable_root_isMapped(_c) \
    cap_page_upper_directory_cap_get_capPUDIsMapped(_c)
#define cap_vtable_cap_new(_a, _v, _m) cap_page_upper_directory_cap_new(_a, _v, _m, 0)
#define vtable_invalid_new(_a, _v) pude_pude_invalid_new(_a, _v)
#define vtable_invalid_get_stored_asid_valid(_v) \
    pude_pude_invalid_get_stored_asid_valid(_v)
#define vtable_invalid_get_stored_hw_asid(_v) pude_pude_invalid_get_stored_hw_asid(_v)

static inline exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr, cte_t *cte)
{
    cap_page_upper_directory_cap_ptr_set_capPUDMappedASID(&cte->cap, asid);
    cap_page_upper_directory_cap_ptr_set_capPUDIsMapped(&cte->cap, 1);
    poolPtr->array[asid & MASK(asidLowBits)] =
        PUDE_PTR(cap_page_upper_directory_cap_get_capPUDBasePtr(cte->cap));

    return EXCEPTION_NONE;
}

#else

#define cap_vtable_root_cap cap_page_global_directory_cap
#define cap_vtable_root_get_mappedASID(_c) \
    cap_page_global_directory_cap_get_capPGDMappedASID(_c)
#define cap_vtable_root_get_basePtr(_c) \
    PGDE_PTR(cap_page_global_directory_cap_get_capPGDBasePtr(_c))
#define cap_vtable_root_isMapped(_c) cap_page_global_directory_cap_get_capPGDIsMapped(_c)
#define cap_vtable_cap_new(_a, _v, _m) \
    cap_page_global_directory_cap_new(_a, _v, _m)
#define vtable_invalid_new(_a, _v) pgde_pgde_invalid_new(_a, _v)
#define vtable_invalid_get_stored_asid_valid(_v) \
    pgde_pgde_invalid_get_stored_asid_valid(_v)
#define vtable_invalid_get_stored_hw_asid(_v) pgde_pgde_invalid_get_stored_hw_asid(_v)

static inline exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr, cte_t *cte)
{
    cap_page_global_directory_cap_ptr_set_capPGDMappedASID(&cte->cap, asid);
    cap_page_global_directory_cap_ptr_set_capPGDIsMapped(&cte->cap, 1);
    poolPtr->array[asid & MASK(asidLowBits)] =
        PGDE_PTR(cap_page_global_directory_cap_get_capPGDBasePtr(cte->cap));

    return EXCEPTION_NONE;
}
#endif