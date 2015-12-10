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

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

void map_it_pt_cap(cap_t pt_cap);
void map_it_frame_cap(cap_t frame_cap);
void map_kernel_window(void);
void map_kernel_frame(paddr_t paddr, pptr_t vaddr, vm_rights_t vm_rights, vm_attributes_t vm_attributes);
void activate_global_pd(void);

/* ==================== BOOT CODE FINISHES HERE ==================== */

/* PD slot reserved for storing the PD's allocated hardware ASID */
#define PD_ASID_SLOT 0xff0

void idle_thread(void);
#define idleThreadStart (&idle_thread)

enum pde_pte_tag {
    ME_PDE,
    ME_PTE
};
typedef word_t pde_pte_tag_t;

struct createMappingEntries_ret {
    exception_t status;
    pde_pte_tag_t tag;
    void *pde_pte_ptr;
    unsigned int offset;
    word_t size;
};
typedef struct createMappingEntries_ret createMappingEntries_ret_t;

struct lookupPTSlot_ret {
    exception_t status;
    pte_t *pt;
    unsigned int ptIndex;
};
typedef struct lookupPTSlot_ret lookupPTSlot_ret_t;

void copyGlobalMappings(pde_t *newPD);
word_t* PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);
lookupPTSlot_ret_t lookupPTSlot(pde_t *pd, vptr_t vptr);
pde_t* CONST lookupPDSlot(pde_t *pd, vptr_t vptr);
exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);
void unmapPageTable(pde_t *pd, uint32_t pdIndex, pte_t* pt);
void unmapPagePTE(vm_page_size_t page_size, pte_t *pt, unsigned int ptIndex, void *pptr);
void unmapPagePDE(vm_page_size_t page_size, pde_t *pd, unsigned int pdIndex, void *pptr);
void unmapAllPages(pde_t *pd, uint32_t pdIndex, pte_t *pt);
void unmapAllPageTables(pde_t *pd);
void setVMRoot(tcb_t *tcb);
bool_t CONST isValidVTableRoot(cap_t cap);
exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);
vm_rights_t CONST maskVMRights(vm_rights_t vm_rights,
                               cap_rights_t cap_rights_mask);
hw_asid_t getHWASID(pde_t *pd);
hw_asid_t findFreeHWASID(void) VISIBLE;
void flushPage(vm_page_size_t page_size, pde_t* pd, word_t vptr);
void flushTable(pde_t* pd, word_t vptr, pte_t* pt);
exception_t decodeARMMMUInvocation(word_t label, word_t length, cptr_t cptr,
                                   cte_t *cte, cap_t cap, extra_caps_t extraCaps,
                                   word_t *buffer);
exception_t performPageTableInvocationMap(cap_t cap, cte_t *ctSlot,
                                          pde_t pde, pde_t *pdSlot);
exception_t performPageTableInvocationUnmap(cap_t cap, cte_t *ctSlot);
exception_t performPageInvocationMapPTE(cap_t cap, cte_t *ctSlot,
                                        pte_t pte, pte_range_t pte_entries);
exception_t performPageInvocationMapPDE(cap_t cap, cte_t *ctSlot,
                                        pde_t pde, pde_range_t pde_entries);
exception_t performPageInvocationUnmap(cap_t cap, cte_t *ctSlot);
#endif
