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

#define IT_ASID 1 /* initial thread's ASID */

void map_it_pt_cap(cap_t pd_cap, cap_t pt_cap);
void map_it_frame_cap(cap_t pd_cap, cap_t frame_cap, bool_t executable);
void map_kernel_window(void);
void map_kernel_frame(paddr_t paddr, pptr_t vaddr, vm_rights_t vm_rights, vm_attributes_t vm_attributes);
void activate_global_pd(void);
void write_it_asid_pool(cap_t it_ap_cap, cap_t it_pd_cap);

/* ==================== BOOT CODE FINISHES HERE ==================== */

/* PD slot reserved for storing the PD's allocated hardware ASID */
#define PD_ASID_SLOT (0xff000000 >> (PT_BITS + PAGE_BITS))

void idle_thread(void);
#define idleThreadStart (&idle_thread)

/* need a fake array to get the pointer from the linker script */
extern char arm_vector_table[1];

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

struct findPDForASID_ret {
    exception_t status;
    pde_t *pd;
};
typedef struct findPDForASID_ret findPDForASID_ret_t;

struct lookupPTSlot_ret {
    exception_t status;
    pte_t *ptSlot;
};
typedef struct lookupPTSlot_ret lookupPTSlot_ret_t;

#ifdef ARM_HYP
hw_asid_t getHWASID(asid_t asid);
#endif
void copyGlobalMappings(pde_t *newPD);
word_t* PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);
findPDForASID_ret_t findPDForASID(asid_t asid) VISIBLE;
lookupPTSlot_ret_t lookupPTSlot(pde_t *pd, vptr_t vptr);
pde_t* CONST lookupPDSlot(pde_t *pd, vptr_t vptr);
exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);
void deleteASIDPool(asid_t base, asid_pool_t* pool);
void deleteASID(asid_t asid, pde_t* pd);
pde_t* pageTableMapped(asid_t asid, vptr_t vaddr, pte_t* pt);
void unmapPageTable(asid_t asid, vptr_t vaddr, pte_t* pt);
void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, void *pptr);
void setVMRoot(tcb_t *tcb);
bool_t CONST isValidVTableRoot(cap_t cap);
exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);
vm_rights_t CONST maskVMRights(vm_rights_t vm_rights,
                               cap_rights_t cap_rights_mask);
hw_asid_t getHWASID(asid_t asid);
hw_asid_t findFreeHWASID(void) VISIBLE;
void flushPage(vm_page_size_t page_size, pde_t* pd, asid_t asid, word_t vptr);
void flushTable(pde_t* pd, asid_t asid, word_t vptr, pte_t* pt);
void flushSpace(asid_t asid);
void invalidateTLBByASID(asid_t asid);
exception_t decodeARMMMUInvocation(word_t invLabel, word_t length, cptr_t cptr,
                                   cte_t *cte, cap_t cap, extra_caps_t excaps,
                                   word_t *buffer);
exception_t performPageTableInvocationMap(cap_t cap, cte_t *ctSlot,
                                          pde_t pde, pde_t *pdSlot);
exception_t performPageTableInvocationUnmap(cap_t cap, cte_t *ctSlot);
exception_t performPageInvocationMapPTE(asid_t asid, cap_t cap, cte_t *ctSlot,
                                        pte_t pte, pte_range_t pte_entries);
exception_t performPageInvocationMapPDE(asid_t asid, cap_t cap, cte_t *ctSlot,
                                        pde_t pde, pde_range_t pde_entries);
exception_t performPageInvocationRemapPTE(asid_t asid, pte_t pte, pte_range_t pte_entries);
exception_t performPageInvocationRemapPDE(asid_t asid, pde_t pde, pde_range_t pde_entries);
exception_t performPageInvocationUnmap(cap_t cap, cte_t *ctSlot);
exception_t performASIDControlInvocation(void *frame, cte_t *slot,
                                         cte_t *parent, asid_t base);
exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t *poolPtr,
                                      cte_t *pdCapSlot);

#endif
