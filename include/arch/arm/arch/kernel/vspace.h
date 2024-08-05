/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <object/structures.h>
#include <mode/kernel/vspace.h>

#define IT_ASID 1 /* initial thread's ASID */

cap_t create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg);
cap_t create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large);
cap_t create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large,
                                 bool_t executable);

void map_kernel_window(void);
void map_kernel_frame(paddr_t paddr, pptr_t vaddr, vm_rights_t vm_rights, vm_attributes_t vm_attributes);
void activate_kernel_vspace(void);
void write_it_asid_pool(cap_t it_ap_cap, cap_t it_pd_cap);

/* ==================== BOOT CODE FINISHES HERE ==================== */

/* need a fake array to get the pointer from the linker script */
extern char arm_vector_table[1];

word_t *PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);
exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);
void setVMRoot(tcb_t *tcb);
bool_t CONST isValidVTableRoot(cap_t cap);
exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);

vm_rights_t CONST maskVMRights(vm_rights_t vm_rights,
                               seL4_CapRights_t cap_rights_mask);

exception_t decodeARMMMUInvocation(word_t invLabel, word_t length, cptr_t cptr,
                                   cte_t *cte, cap_t cap, bool_t call, word_t *buffer);

#ifdef CONFIG_PRINTING
void Arch_userStackTrace(tcb_t *tptr);
#endif

