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

struct lookupPDSlot_ret {
    exception_t     status;
    pde_t*          pdSlot;
    pde_t*          pd;
    unsigned int    pdIndex;
};
typedef struct lookupPDSlot_ret lookupPDSlot_ret_t;

void init_boot_pd(void);
void enable_paging(void);
bool_t map_kernel_window(
    pdpte_t*   pdpt,
    pde_t*     pd,
    pte_t*     pt,
    p_region_t ndks_p_reg
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
void map_it_pt_cap(cap_t pt_cap);
void map_it_pd_cap(cap_t pd_cap);
void map_it_frame_cap(cap_t frame_cap);
bool_t init_pat_msr(void);

/* ==================== BOOT CODE FINISHES HERE ==================== */

void idle_thread(void);
#define idleThreadStart (&idle_thread)

bool_t isVTableRoot(cap_t cap);
lookupPDSlot_ret_t lookupPDSlot(void *vspace, vptr_t vptr);
void copyGlobalMappings(void* new_vspace);
word_t* PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);
exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);
void flushAllPageTables(pde_t *pd);
void unmapAllPageDirectories(pdpte_t *pdpt);
void flushAllPageDirectories(pdpte_t *pdpt);
void unmapPageDirectory(pdpte_t *pdpt, uint32_t pdptIndex, pde_t *pd);
void unmapAllPageTables(pde_t *pd);
void unmapPageTable(pde_t *pd, uint32_t pdIndex);
void unmapAllPages(pte_t *pt);
void unmapPageSmall(pte_t *pt, uint32_t ptIndex);
void flushPageSmall(pte_t *pt, uint32_t ptIndex);
void unmapPageLarge(pde_t *pd, uint32_t pdIndex);
void flushPageLarge(pde_t *pd, uint32_t pdIndex);
void flushPageDirectory(pdpte_t *pdpt, uint32_t pdptIndex, pde_t *pd);
void setVMRoot(tcb_t *tcb);
bool_t CONST isValidVTableRoot(cap_t cap);
bool_t CONST isValidNativeRoot(cap_t cap);
exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);
vm_rights_t CONST maskVMRights(vm_rights_t vm_rights, cap_rights_t cap_rights_mask);
exception_t decodeIA32MMUInvocation(word_t label, word_t length, cptr_t cptr, cte_t *cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
void *mapSpare4MFrame(paddr_t phys);
exception_t decodeIA32PageDirectoryInvocation(word_t label, word_t length, cte_t* cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
void unmapSpare4MFrame(void);
void *mapSpare4KFrame(paddr_t phyS);
void unmapSpare4KFrame(void);

#ifdef CONFIG_VTX
void unmapEPTPD(ept_pdpte_t *pdpt, uint32_t index, ept_pde_t *pd);
void unmapEPTPT(ept_pde_t *pd, uint32_t index, ept_pte_t *pt);
exception_t decodeIA32EPTInvocation( word_t label, unsigned int length, cptr_t cptr, cte_t* cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
exception_t decodeIA32EPTPageDirectoryInvocation( word_t label, unsigned int length, cte_t* cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
exception_t decodeIA32EPTPageTableInvocation( word_t label, unsigned int length, cte_t* cte, cap_t cap, extra_caps_t extraCaps, word_t* buffer);
void IA32EptPdpt_Init(ept_pml4e_t *pdpt);
ept_pdpte_t *lookupEPTPDPTFromPD(ept_pde_t *pt);
void IA32PageUnmapEPT(cap_t cap);
void unmapAllEPTPD(ept_pdpte_t *pdpt);
void unmapAllEPTPT(ept_pde_t *pd);
void unmapAllEPTPages(ept_pte_t *pt);
#endif /* VTX */
#endif
