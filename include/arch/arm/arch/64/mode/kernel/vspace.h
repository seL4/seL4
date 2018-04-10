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

#ifndef __ARCH_MODE_KERNEL_VSPACE_H_
#define __ARCH_MODE_KERNEL_VSPACE_H_

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#define activate_global_pd activate_kernel_vspace

/* ==================== BOOT CODE FINISHES HERE ==================== */

bool_t CONST isVTableRoot(cap_t cap);
bool_t CONST isValidNativeRoot(cap_t cap);

pgde_t *pageUpperDirectoryMapped(asid_t asid, vptr_t vaddr, pude_t* pud);
pude_t *pageDirectoryMapped(asid_t asid, vptr_t vaddr, pde_t* pd);
void unmapPageUpperDirectory(asid_t asid, vptr_t vaddr, pude_t* pud);
void unmapPageDirectory(asid_t asid, vptr_t vaddr, pde_t* pd);

void unmapPageTable(asid_t asid, vptr_t vaddr, pte_t *pt);
void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, pptr_t pptr);

void deleteASIDPool(asid_t base, asid_pool_t* pool);
void deleteASID(asid_t asid, vspace_root_t *vspace);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
hw_asid_t getHWASID(asid_t asid);
#endif

/* Reserved memory ranges */
static const region_t BOOT_RODATA mode_reserved_region[] = {};

#endif /* __ARCH_MODE_KERNEL_VSPACE_H_ */
