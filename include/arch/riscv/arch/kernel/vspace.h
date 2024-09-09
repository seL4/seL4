/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

cap_t create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg);
void map_it_pt_cap(cap_t vspace_cap, cap_t pt_cap);
void map_it_frame_cap(cap_t vspace_cap, cap_t frame_cap);
void map_kernel_window(void);
void map_kernel_frame(paddr_t paddr, pptr_t vaddr, vm_rights_t vm_rights);
void activate_kernel_vspace(void);
void write_it_asid_pool(cap_t it_ap_cap, cap_t it_lvl1pt_cap);


/* ==================== BOOT CODE FINISHES HERE ==================== */
#define IT_ASID 1

struct lookupPTSlot_ret {
    pte_t *ptSlot;
    word_t ptBitsLeft;
};

typedef struct lookupPTSlot_ret lookupPTSlot_ret_t;

struct findVSpaceForASID_ret {
    exception_t status;
    pte_t *vspace_root;
};
typedef struct findVSpaceForASID_ret findVSpaceForASID_ret_t;

void copyGlobalMappings(pte_t *newlvl1pt);
register_t *PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);
lookupPTSlot_ret_t lookupPTSlot(pte_t *lvl1pt, vptr_t vptr);
exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);
void unmapPageTable(asid_t, vptr_t vaddr, pte_t *pt);
void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, pptr_t pptr);
void deleteASID(asid_t asid, pte_t *vspace);
void deleteASIDPool(asid_t asid_base, asid_pool_t *pool);
bool_t CONST isValidVTableRoot(cap_t cap);
exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);
vm_rights_t CONST maskVMRights(vm_rights_t vm_rights,
                               seL4_CapRights_t cap_rights_mask);
exception_t decodeRISCVMMUInvocation(word_t label, word_t length, cptr_t cptr,
                                     cte_t *cte, cap_t cap, bool_t call, register_t *buffer);
exception_t performPageTableInvocationMap(cap_t cap, cte_t *ctSlot,
                                          pte_t lvl1pt, pte_t *ptSlot);
exception_t performPageTableInvocationUnmap(cap_t cap, cte_t *ctSlot);
exception_t performPageInvocationMapPTE(cap_t cap, cte_t *ctSlot,
                                        pte_t pte, pte_t *base);
exception_t performPageInvocationUnmap(cap_t cap, cte_t *ctSlot);
void setVMRoot(tcb_t *tcb);

#ifdef CONFIG_PRINTING
void Arch_userStackTrace(tcb_t *tptr);
#endif

