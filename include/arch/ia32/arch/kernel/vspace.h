/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_VSPACE_H
#define __ARCH_KERNEL_VSPACE_H

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#define IT_ASID 1 /* initial thread's ASID */

void init_boot_pd(void);
void enable_paging(void);
bool_t map_kernel_window(
    pde_t*     pd,
    pte_t*     pt,
    p_region_t ndks_p_reg
#ifdef CONFIG_IOMMU
    , uint32_t   num_drhu,
    paddr_t*   drhu_list
#endif
);

void* map_temp_boot_page(void* entry, uint32_t pages);
bool_t init_vm_state(pde_t* kernel_pd, pte_t* kernel_pt);
void init_dtrs(void);
void map_it_pt_cap(cap_t pd_cap, cap_t pt_cap);
void map_it_frame_cap(cap_t pd_cap, cap_t frame_cap);
void write_it_asid_pool(cap_t it_ap_cap, cap_t it_pd_cap);
bool_t init_pat_msr(void);

/* ==================== BOOT CODE FINISHES HERE ==================== */

void idle_thread(void);
#define idleThreadStart (&idle_thread)

void copyGlobalMappings(pde_t* newPD);
word_t* PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);
exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);
void unmapPageTable(asid_t, vptr_t vaddr, pte_t* pt);
void deleteASIDPool(asid_t asid_base, asid_pool_t* pool);
void deleteASID(asid_t asid, pde_t* pd);
void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, void *pptr);
void setVMRoot(tcb_t *tcb);
bool_t CONST isValidVTableRoot(cap_t cap);
exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);
vm_rights_t CONST maskVMRights(vm_rights_t vm_rights, cap_rights_t cap_rights_mask);
exception_t decodeIA32MMUInvocation(word_t label, unsigned int length, cptr_t cptr, cte_t *cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);

#endif
