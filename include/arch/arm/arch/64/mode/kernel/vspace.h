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

asid_map_t findMapForASID(asid_t asid);

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

#ifdef CONFIG_ARM_SMMU
#define cap_vtable_root_get_mappedCB(_c) \
    cap_page_upper_directory_cap_get_capPUDMappedCB(_c)
#define cap_vtable_root_ptr_set_mappedCB(_c, cb) \
    cap_page_upper_directory_cap_ptr_set_capPUDMappedCB(_c, cb)
#define cap_vtable_cap_new(_a, _v, _m) cap_page_upper_directory_cap_new(_a, _v, _m, 0, CB_INVALID)
#else
#define cap_vtable_cap_new(_a, _v, _m) cap_page_upper_directory_cap_new(_a, _v, _m, 0)
#endif  /*!CONFIG_ARM_SMMU*/

#define vtable_invalid_get_stored_asid_valid(_v) \
    pude_pude_invalid_get_stored_asid_valid(_v)
#define vtable_invalid_get_stored_hw_asid(_v) pude_pude_invalid_get_stored_hw_asid(_v)

static inline exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr, cte_t *cte)
{
    cap_page_upper_directory_cap_ptr_set_capPUDMappedASID(&cte->cap, asid);
    cap_page_upper_directory_cap_ptr_set_capPUDIsMapped(&cte->cap, 1);
    asid_map_t asid_map = asid_map_asid_map_vspace_new(
#ifdef CONFIG_ARM_SMMU
                              /* bind_cb: Number of bound context banks */
                              0,
#endif
                              /* vspace_root: reference to vspace root page table object */
                              cap_page_upper_directory_cap_get_capPUDBasePtr(cte->cap)
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                              /* stored_hw_vmid, stored_vmid_valid: Assigned hardware VMID for TLB. */
                              , 0, false
#endif
                          );
    poolPtr->array[asid & MASK(asidLowBits)] = asid_map;
    return EXCEPTION_NONE;
}


#else

#define cap_vtable_root_cap cap_page_global_directory_cap
#define cap_vtable_root_get_mappedASID(_c) \
    cap_page_global_directory_cap_get_capPGDMappedASID(_c)
#define cap_vtable_root_get_basePtr(_c) \
    PGDE_PTR(cap_page_global_directory_cap_get_capPGDBasePtr(_c))
#define cap_vtable_root_isMapped(_c) cap_page_global_directory_cap_get_capPGDIsMapped(_c)

#ifdef CONFIG_ARM_SMMU
#define cap_vtable_root_get_mappedCB(_c) \
    cap_page_global_directory_cap_get_capPGDMappedCB(_c)
#define cap_vtable_root_ptr_set_mappedCB(_c, cb) \
    cap_page_global_directory_cap_ptr_set_capPGDMappedCB(_c, cb)
#define cap_vtable_cap_new(_a, _v, _m) \
    cap_page_global_directory_cap_new(_a, _v, _m, CB_INVALID)
#else
#define cap_vtable_cap_new(_a, _v, _m) \
    cap_page_global_directory_cap_new(_a, _v, _m)
#endif /*!CONFIG_ARM_SMMU*/

#define vtable_invalid_get_stored_asid_valid(_v) \
    pgde_pgde_invalid_get_stored_asid_valid(_v)
#define vtable_invalid_get_stored_hw_asid(_v) pgde_pgde_invalid_get_stored_hw_asid(_v)

static inline exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr, cte_t *cte)
{
    cap_page_global_directory_cap_ptr_set_capPGDMappedASID(&cte->cap, asid);
    cap_page_global_directory_cap_ptr_set_capPGDIsMapped(&cte->cap, 1);
    asid_map_t asid_map = asid_map_asid_map_vspace_new(
#ifdef CONFIG_ARM_SMMU
                              /* bind_cb: Number of bound context banks */
                              0,
#endif
                              /* vspace_root: reference to vspace root page table object */
                              cap_page_global_directory_cap_get_capPGDBasePtr(cte->cap)
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
                              /* stored_hw_vmid, stored_vmid_valid: Assigned hardware VMID for TLB. */
                              , 0, false
#endif
                          );

    poolPtr->array[asid & MASK(asidLowBits)] = asid_map;

    return EXCEPTION_NONE;
}

void increaseASIDBindCB(asid_t asid);
void decreaseASIDBindCB(asid_t asid);

#endif
