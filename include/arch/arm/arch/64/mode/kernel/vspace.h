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

#define MODE_RESERVED 0

/* ==================== BOOT CODE FINISHES HERE ==================== */

bool_t CONST isVTableRoot(cap_t cap);
bool_t CONST isValidNativeRoot(cap_t cap);

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

#define PAR_EL1_MASK 0x0000fffffffff000ul
#define GET_PAR_ADDR(x) ((x) & PAR_EL1_MASK)

static inline exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr, cte_t *cte)
{
    cap_vspace_cap_ptr_set_capMappedASID(&cte->cap, asid);
    cap_vspace_cap_ptr_set_capIsMapped(&cte->cap, 1);
    asid_map_t asid_map = asid_map_asid_map_vspace_new(
#ifdef CONFIG_ARM_SMMU
                              /* bind_cb: Number of bound context banks */
                              0,
#endif
                              /* vspace_root: reference to vspace root page table object */
                              cap_vspace_cap_get_capPTBasePtr(cte->cap)
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
