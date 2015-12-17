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

struct lookupPDSlot_ret {
    exception_t status;
    pde_t*      pdSlot;
};
typedef struct lookupPDSlot_ret lookupPDSlot_ret_t;

struct findVSpaceForASID_ret {
    exception_t status;
    void *vspace_root;
};
typedef struct findVSpaceForASID_ret findVSpaceForASID_ret_t;

void init_boot_pd(void);
void enable_paging(void);
bool_t map_kernel_window(
    pdpte_t*   pdpt,
    pde_t*     pd,
    pte_t*     pt
#ifdef CONFIG_IRQ_IOAPIC
    , uint32_t num_ioapic,
    paddr_t*   ioapic_paddrs
#endif
#ifdef CONFIG_IOMMU
    , uint32_t   num_drhu,
    paddr_t*   drhu_list
#endif
);

void *getValidNativeRoot(cap_t vspace_cap);
pde_t *get_boot_pd(void);
void* map_temp_boot_page(void* entry, uint32_t large_pages);
bool_t init_vm_state(pdpte_t *kernel_pdpt, pde_t* kernel_pd, pte_t* kernel_pt);
void init_dtrs(void);
void map_it_pt_cap(cap_t vspace_cap, cap_t pt_cap);
void map_it_pd_cap(cap_t vspace_cap, cap_t pd_cap);
void map_it_frame_cap(cap_t vspace_cap, cap_t frame_cap);
void write_it_asid_pool(cap_t it_ap_cap, cap_t it_vspace_cap);
bool_t init_pat_msr(void);

/* ==================== BOOT CODE FINISHES HERE ==================== */

void idle_thread(void);
#define idleThreadStart (&idle_thread)

bool_t isVTableRoot(cap_t cap);
exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t* poolPtr, cte_t* vspaceCapSlot);
lookupPDSlot_ret_t lookupPDSlot(void *vspace, vptr_t vptr);
void copyGlobalMappings(void* new_vspace);
word_t* PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);
exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);
void unmapPageDirectory(asid_t asid, vptr_t vaddr, pde_t *pd);
void unmapPageTable(asid_t, vptr_t vaddr, pte_t* pt);
void deleteASIDPool(asid_t asid_base, asid_pool_t* pool);
void deleteASID(asid_t asid, void* vspace);
findVSpaceForASID_ret_t findVSpaceForASID(asid_t asid);
void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, void *pptr);
void setVMRoot(tcb_t *tcb);
bool_t CONST isValidVTableRoot(cap_t cap);
bool_t CONST isValidNativeRoot(cap_t cap);
exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);
vm_rights_t CONST maskVMRights(vm_rights_t vm_rights, cap_rights_t cap_rights_mask);
exception_t decodeIA32MMUInvocation(word_t label, word_t length, cptr_t cptr, cte_t *cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
exception_t decodeIA32PageDirectoryInvocation(word_t label, word_t length, cte_t* cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);

#endif
