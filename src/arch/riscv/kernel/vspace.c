/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 * Copyright (c) 2018, Hesham Almatary <Hesham.Almatary@cl.cam.ac.uk>
 * All rights reserved.
 *
 * This software was was developed in part by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#include <types.h>
#include <benchmark/benchmark.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <kernel/boot.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <object/tcb.h>
#include <machine/io.h>
#include <model/preemption.h>
#include <model/statedata.h>
#include <object/cnode.h>
#include <object/untyped.h>
#include <arch/api/invocation.h>
#include <arch/kernel/vspace.h>
#include <linker.h>
#include <plat/machine/devices.h>
#include <arch/machine.h>
#include <plat/machine/hardware.h>
#include <kernel/stack.h>

struct resolve_ret {
    paddr_t frameBase;
    vm_page_size_t frameSize;
    bool_t valid;
};
typedef struct resolve_ret resolve_ret_t;

static exception_t performPageGetAddress(void *vbase_ptr);

static word_t CONST
RISCVGetWriteFromVMRights(vm_rights_t vm_rights)
{
    return vm_rights != VMReadOnly;
}

static word_t RISCVGetUserFromVMRights(vm_rights_t vm_rights)
{
    return vm_rights != VMKernelOnly;
}

static inline word_t CONST
RISCVGetReadFromVMRights(vm_rights_t vm_rights)
{
    return vm_rights != VMWriteOnly;
}

/* ==================== BOOT CODE STARTS HERE ==================== */

BOOT_CODE VISIBLE void
map_kernel_window(void)
{
    /* mapping of kernelBase (virtual address) to kernel's physBase  */
    assert(CONFIG_PT_LEVELS > 1 && CONFIG_PT_LEVELS <= 4);

    /* kernel window starts at PPTR_BASE */
    word_t pptr = PPTR_BASE;

    /* first we map in memory from PADDR_BASE */
    word_t paddr = PADDR_BASE;
    while (pptr < KERNEL_BASE) {
        assert(IS_ALIGNED(pptr, RISCV_GET_LVL_PGSIZE_BITS(1)));
        assert(IS_ALIGNED(paddr, RISCV_GET_LVL_PGSIZE_BITS(1)));
        kernel_pageTables[0][RISCV_GET_PT_INDEX(pptr, 1)] =
            pte_new(
                paddr >> seL4_PageBits,
                0,  /* sw */
                1,  /* dirty */
                1,  /* accessed */
                1,  /* global */
                0,  /* user */
                1,  /* execute */
                1,  /* write */
                1,  /* read */
                1   /* valid */
            );
        pptr += RISCV_GET_LVL_PGSIZE(1);
        paddr += RISCV_GET_LVL_PGSIZE(1);
    }
    /* now we should be mapping the kernel base, starting again from PADDR_LOAD */
    assert(pptr == KERNEL_BASE);
    paddr = PADDR_LOAD;
    /* pptr will overflow at the end of the address range so this loop condition looks odd */
    while (pptr >= KERNEL_BASE) {
        assert(IS_ALIGNED(pptr, RISCV_GET_LVL_PGSIZE_BITS(1)));
        assert(IS_ALIGNED(paddr, RISCV_GET_LVL_PGSIZE_BITS(1)));
        kernel_pageTables[0][RISCV_GET_PT_INDEX(pptr, 1)] =
            pte_new(
                paddr >> seL4_PageBits,
                0,  /* sw */
                1,  /* dirty */
                1,  /* accessed */
                1,  /* global */
                0,  /* user */
                1,  /* execute */
                1,  /* write */
                1,  /* read */
                1   /* valid */
            );
        pptr += RISCV_GET_LVL_PGSIZE(1);
        paddr += RISCV_GET_LVL_PGSIZE(1);
    }
}

BOOT_CODE void
map_it_pt_cap(cap_t vspace_cap, cap_t pt_cap)
{
    lookupPTSlot_ret_t pt_ret;
    pte_t* targetSlot;
    vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(pt_cap);
    pte_t* lvl1pt = PTE_PTR(pptr_of_cap(vspace_cap));

    /* pt to be mapped */
    pte_t* pt   = PTE_PTR(pptr_of_cap(pt_cap));

    /* Get PT slot to install the address in */
    pt_ret = lookupPTSlot(lvl1pt, vptr);

    targetSlot = pt_ret.ptSlot;

    *targetSlot = pte_new(
                      (addrFromPPtr(pt) >> seL4_PageBits),
                      0, /* sw */
                      1, /* dirty */
                      1, /* accessed */
                      0,  /* global */
                      0,  /* user */
                      0,  /* execute */
                      0,  /* write */
                      0,  /* read */
                      1 /* valid */
                  );
    sfence();
}

BOOT_CODE void
map_it_frame_cap(cap_t vspace_cap, cap_t frame_cap)
{
    pte_t* lvl1pt   = PTE_PTR(pptr_of_cap(vspace_cap));
    pte_t* frame_pptr   = PTE_PTR(pptr_of_cap(frame_cap));
    vptr_t frame_vptr = cap_frame_cap_get_capFMappedAddress(frame_cap);

    /* We deal with a frame as 4KiB */
    lookupPTSlot_ret_t lu_ret = lookupPTSlot(lvl1pt, frame_vptr);
    assert(lu_ret.ptBitsLeft == seL4_PageBits);

    pte_t* targetSlot = lu_ret.ptSlot;

    *targetSlot = pte_new(
                      (pptr_to_paddr(frame_pptr) >> seL4_PageBits),
                      0, /* sw */
                      1, /* dirty */
                      1, /* accessed */
                      0,  /* global */
                      1,  /* user */
                      1,  /* execute */
                      1,  /* write */
                      1,  /* read */
                      1   /* valid */
                  );
    sfence();
}

BOOT_CODE cap_t
create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large)
{
    cap_t cap = cap_frame_cap_new(
                    asidInvalid,                     /* capFMappedASID       */
                    pptr,                            /* capFBasePtr          */
                    0,                               /* capFSize             */
                    0,                               /* capFVMRights         */
                    0,
                    0                                /* capFMappedAddress    */
                );

    return cap;
}

/* Create a page table for the initial thread */
static BOOT_CODE cap_t
create_it_pt_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_table_cap_new(
              asid,   /* capPTMappedASID      */
              pptr,   /* capPTBasePtr         */
              1,      /* capPTIsMapped        */
              vptr    /* capPTMappedAddress   */
          );

    map_it_pt_cap(vspace_cap, cap);
    return cap;
}

/* Create an address space for the initial thread.
 * This includes page directory and page tables */
BOOT_CODE cap_t
create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg)
{
    cap_t      lvl1pt_cap;
    vptr_t     pt_vptr;
    pptr_t     pt_pptr;
    pptr_t lvl1pt_pptr;

    /* create 1st level page table obj and cap */
    lvl1pt_pptr = alloc_region(PT_SIZE_BITS);

    if (!lvl1pt_pptr) {
        return cap_null_cap_new();
    }
    memzero(PTE_PTR(lvl1pt_pptr), 1 << PT_SIZE_BITS);

    if (!config_set(CONFIG_SEL4_RV_MACHINE)) {
        copyGlobalMappings(PTE_PTR(lvl1pt_pptr));
    }

    lvl1pt_cap =
        cap_page_table_cap_new(
            IT_ASID,               /* capPTMappedASID    */
            (word_t) lvl1pt_pptr,  /* capPTBasePtr       */
            1,                     /* capPTIsMapped      */
            (word_t) lvl1pt_pptr   /* capPTMappedAddress */
        );

    seL4_SlotPos slot_pos_before = ndks_boot.slot_pos_cur;
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadVSpace), lvl1pt_cap);

    /* create all n level PT objs and caps necessary to cover userland image in 4KiB pages */

    for (int i = 2; i <= CONFIG_PT_LEVELS; i++) {

        for (pt_vptr = ROUND_DOWN(it_v_reg.start, RISCV_GET_LVL_PGSIZE_BITS(i - 1));
                pt_vptr < it_v_reg.end;
                pt_vptr += RISCV_GET_LVL_PGSIZE(i - 1)) {
            pt_pptr = alloc_region(PT_SIZE_BITS);

            if (!pt_pptr) {
                return cap_null_cap_new();
            }

            memzero(PTE_PTR(pt_pptr), 1 << PT_SIZE_BITS);
            if (!provide_cap(root_cnode_cap,
                             create_it_pt_cap(lvl1pt_cap, pt_pptr, pt_vptr, IT_ASID))
               ) {
                return cap_null_cap_new();
            }
        }

    }

    seL4_SlotPos slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->userImagePaging = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };

    return lvl1pt_cap;
}

BOOT_CODE void
activate_kernel_vspace(void)
{
    setVSpaceRoot(kpptr_to_paddr(&kernel_pageTables[0]), 0);
}

BOOT_CODE void
write_it_asid_pool(cap_t it_ap_cap, cap_t it_lvl1pt_cap)
{
    asid_pool_t* ap = ASID_POOL_PTR(pptr_of_cap(it_ap_cap));
    ap->array[IT_ASID] = PTE_PTR(pptr_of_cap(it_lvl1pt_cap));
    riscvKSASIDTable[IT_ASID >> asidLowBits] = ap;
}

/* ==================== BOOT CODE FINISHES HERE ==================== */

static findVSpaceForASID_ret_t findVSpaceForASID(asid_t asid)
{
    findVSpaceForASID_ret_t ret;
    asid_pool_t*        poolPtr;
    pte_t*     vspace_root;

    poolPtr = riscvKSASIDTable[asid >> asidLowBits];
    if (!poolPtr) {
        current_lookup_fault = lookup_fault_invalid_root_new();

        ret.vspace_root = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    vspace_root = poolPtr->array[asid & MASK(asidLowBits)];
    if (!vspace_root) {
        //current_lookup_fault = lookup_fault_invalid_root_new();
        current_lookup_fault = lookup_fault_missing_capability_new(RISCV_GET_LVL_PGSIZE_BITS(1));

        ret.vspace_root = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ret.vspace_root = vspace_root;
    ret.status = EXCEPTION_NONE;
    return ret;
}

void
copyGlobalMappings(pte_t *newLvl1pt)
{
    unsigned int i;
    pte_t *global_kernel_vspace = kernel_pageTables[0];

    for (i = RISCV_GET_PT_INDEX(PPTR_BASE, 1); i < BIT(PT_INDEX_BITS); i++) {
        newLvl1pt[i] = global_kernel_vspace[i];
    }
}

word_t * PURE
lookupIPCBuffer(bool_t isReceiver, tcb_t *thread)
{
    word_t w_bufferPtr;
    cap_t bufferCap;
    vm_rights_t vm_rights;

    w_bufferPtr = thread->tcbIPCBuffer;
    bufferCap = TCB_PTR_CTE_PTR(thread, tcbBuffer)->cap;

    if (unlikely(cap_get_capType(bufferCap) != cap_frame_cap &&
                 cap_get_capType(bufferCap) != cap_frame_cap)) {
        return NULL;
    }
    if (unlikely(cap_frame_cap_get_capFIsDevice(bufferCap))) {
        return NULL;
    }

    vm_rights = cap_frame_cap_get_capFVMRights(bufferCap);
    if (likely(vm_rights == VMReadWrite ||
               (!isReceiver && vm_rights == VMReadOnly))) {
        word_t basePtr;
        unsigned int pageBits;

        basePtr = cap_frame_cap_get_capFBasePtr(bufferCap);
        pageBits = pageBitsForSize(cap_frame_cap_get_capFSize(bufferCap));
        return (word_t *)(basePtr + (w_bufferPtr & MASK(pageBits)));
    } else {
        return NULL;
    }
}

static inline pptr_t isPTEPageTable(pte_t *pte)
{
    return pte_ptr_get_valid(pte) &&
           !(pte_ptr_get_read(pte) || pte_ptr_get_write(pte) || pte_ptr_get_execute(pte));
}

static inline pte_t *getPPtrFromHWPTE(pte_t *pte)
{
    return PTE_PTR(ptrFromPAddr(pte_ptr_get_ppn(pte) << seL4_PageTableBits));
}

lookupPTSlot_ret_t
lookupPTSlot(pte_t *lvl1pt, vptr_t vptr)
{
    lookupPTSlot_ret_t ret;
    /* this is how many bits we have decoded before reaching an empty
     * page table entry or a frame mapping */
    ret.ptBitsLeft = PT_INDEX_BITS * CONFIG_PT_LEVELS + seL4_PageBits;
    ret.ptSlot = NULL;

    pte_t* pt = lvl1pt;
    do {
        ret.ptBitsLeft -= PT_INDEX_BITS;
        word_t index = (vptr >> ret.ptBitsLeft) & MASK(PT_INDEX_BITS);
        ret.ptSlot = pt + index;
        pt = getPPtrFromHWPTE(ret.ptSlot);
        /* stop when we find something that isn't a page table - either a mapped frame or
         * an empty slot */
    } while (isPTEPageTable(ret.ptSlot));

    return ret;
}

exception_t
handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType)
{
    word_t addr = read_csr_env(badaddr);

    switch (vm_faultType) {
    case RISCVLoadPageFault:
    case RISCVLoadAccessFault:
        current_fault = seL4_Fault_VMFault_new(addr, RISCVLoadAccessFault, false);
        return EXCEPTION_FAULT;
    case RISCVStorePageFault:
    case RISCVStoreAccessFault:
        current_fault = seL4_Fault_VMFault_new(addr, RISCVStoreAccessFault, false);
        return EXCEPTION_FAULT;
    case RISCVInstructionPageFault:
    case RISCVInstructionAccessFault:
        setRegister(thread, NEXTPC, getRegister(thread, EPC));
        current_fault = seL4_Fault_VMFault_new(addr, RISCVInstructionAccessFault, true);
        return EXCEPTION_FAULT;

    default:
        fail("Invalid VM fault type");
    }
}

void deleteASIDPool(asid_t asid_base, asid_pool_t* pool)
{
    /* Haskell error: "ASID pool's base must be aligned" */
    assert(IS_ALIGNED(asid_base, asidLowBits));

    if (riscvKSASIDTable[asid_base >> asidLowBits] == pool) {
        riscvKSASIDTable[asid_base >> asidLowBits] = NULL;
        setVMRoot(NODE_STATE(ksCurThread));
    }
}

static exception_t performASIDControlInvocation(void* frame, cte_t* slot, cte_t* parent, asid_t asid_base)
{
    cap_untyped_cap_ptr_set_capFreeIndex(&(parent->cap),
                                         MAX_FREE_INDEX(cap_untyped_cap_get_capBlockSize(parent->cap)));

    memzero(frame, 1 << pageBitsForSize(RISCV_4K_Page));
    cteInsert(
        cap_asid_pool_cap_new(
            asid_base,          /* capASIDBase  */
            WORD_REF(frame)     /* capASIDPool  */
        ),
        parent,
        slot
    );
    /* Haskell error: "ASID pool's base must be aligned" */
    assert((asid_base & MASK(asidLowBits)) == 0);
    riscvKSASIDTable[asid_base >> asidLowBits] = (asid_pool_t*)frame;

    return EXCEPTION_NONE;
}

static exception_t performASIDPoolInvocation(asid_t asid, asid_pool_t* poolPtr, cte_t* vspaceCapSlot)
{
    pte_t *regionBase = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(vspaceCapSlot->cap));
    cap_t cap = vspaceCapSlot->cap;
    cap = cap_page_table_cap_set_capPTMappedASID(cap, asid);
    cap = cap_page_table_cap_set_capPTIsMapped(cap, 1);
    vspaceCapSlot->cap = cap;

    if (!config_set(CONFIG_SEL4_RV_MACHINE)) {
        copyGlobalMappings(regionBase);
    }

    poolPtr->array[asid & MASK(asidLowBits)] = regionBase;

    return EXCEPTION_NONE;
}

void deleteASID(asid_t asid, pte_t *vspace)
{
    asid_pool_t* poolPtr;

    poolPtr = riscvKSASIDTable[asid >> asidLowBits];
    if (poolPtr != NULL && poolPtr->array[asid & MASK(asidLowBits)] == vspace) {
        hwASIDFlush(asid);
        poolPtr->array[asid & MASK(asidLowBits)] = NULL;
        setVMRoot(NODE_STATE(ksCurThread));
    }
}

void
unmapPageTable(asid_t asid, vptr_t vptr, pte_t* target_pt)
{
    findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE)) {
        /* nothing to do */
        return;
    }

    pte_t *ptSlot = find_ret.vspace_root + RISCV_GET_PT_INDEX(vptr, 1);
    pte_t *pt = find_ret.vspace_root;

    for (int i = 2; i <= CONFIG_PT_LEVELS; i++) {
        if (unlikely(!isPTEPageTable(ptSlot))) {
            /* couldn't find it */
            return;
        }
        pt = getPPtrFromHWPTE(ptSlot);
        if (pt == target_pt) {
            /* Found the PT Slot */
            ptSlot = pt + RISCV_GET_PT_INDEX(vptr, i - 1);
            break;
        }
        ptSlot = pt + RISCV_GET_PT_INDEX(vptr, i);
    }

    if (pt != target_pt) {
        /* didn't find it */
        return;
    }

    *ptSlot = pte_new(
                  0,  /* phy_address */
                  0,  /* sw */
                  0,  /* dirty */
                  0,  /* accessed */
                  0,  /* global */
                  0,  /* user */
                  0,  /* execute */
                  0,  /* write */
                  0,  /* read */
                  0  /* valid */
              );
    sfence();
}

static pte_t pte_pte_invalid_new(void)
{
    return (pte_t) {
        0
    };
}

void
unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, pptr_t pptr)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPTSlot_ret_t  lu_ret;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    lu_ret = lookupPTSlot(find_ret.vspace_root, vptr);
    if (unlikely(lu_ret.ptBitsLeft != pageBitsForSize(page_size))) {
        return;
    }

    lu_ret.ptSlot[0] = pte_pte_invalid_new();
    sfence();
}

void
setVMRoot(tcb_t *tcb)
{
    cap_t threadRoot;
    asid_t asid;
    pte_t *lvl1pt;
    findVSpaceForASID_ret_t  find_ret;

    threadRoot = TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap;

    if (cap_get_capType(threadRoot) != cap_page_table_cap) {
        setVSpaceRoot(kpptr_to_paddr(&kernel_pageTables[0]), 0);
        return;
    }

    lvl1pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(threadRoot));

    asid = cap_page_table_cap_get_capPTMappedASID(threadRoot);
    find_ret = findVSpaceForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE || find_ret.vspace_root != lvl1pt)) {
        setVSpaceRoot(kpptr_to_paddr(&kernel_pageTables[0]), asid);
        return;
    }

    setVSpaceRoot(addrFromPPtr(lvl1pt), asid);
}

bool_t CONST
isValidVTableRoot(cap_t cap)
{
    return (cap_get_capType(cap) == cap_page_table_cap &&
            cap_page_table_cap_get_capPTMappedASID(cap) != asidInvalid);
}

exception_t
checkValidIPCBuffer(vptr_t vptr, cap_t cap)
{
    if (unlikely(cap_get_capType(cap) != cap_frame_cap)) {
        userError("Requested IPC Buffer is not a frame cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cap_frame_cap_get_capFIsDevice(cap))) {
        userError("Specifying a device frame as an IPC buffer is not permitted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(!IS_ALIGNED(vptr, seL4_IPCBufferSizeBits))) {
        userError("Requested IPC Buffer location 0x%x is not aligned.",
                  (int)vptr);
        current_syscall_error.type = seL4_AlignmentError;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return EXCEPTION_NONE;
}

vm_rights_t CONST
maskVMRights(vm_rights_t vm_rights, seL4_CapRights_t cap_rights_mask)
{
    if (vm_rights == VMReadOnly &&
            seL4_CapRights_get_capAllowRead(cap_rights_mask)) {
        return VMReadOnly;
    }
    if (vm_rights == VMReadWrite &&
            (seL4_CapRights_get_capAllowRead(cap_rights_mask) || seL4_CapRights_get_capAllowWrite(cap_rights_mask))) {
        if (!seL4_CapRights_get_capAllowWrite(cap_rights_mask)) {
            return VMReadOnly;
        } else if (!seL4_CapRights_get_capAllowRead(cap_rights_mask)) {
            return VMWriteOnly;
        } else {
            return VMReadWrite;
        }
    }
    if (vm_rights == VMWriteOnly &&
            seL4_CapRights_get_capAllowWrite(cap_rights_mask)) {
        return VMWriteOnly;
    }
    if (vm_rights == VMKernelOnly) {
        return VMKernelOnly;
    }
    return VMKernelOnly;
}

/* The rest of the file implements the RISCV object invocations */

static pte_t CONST
makeUserPTE(paddr_t paddr, bool_t executable, vm_rights_t vm_rights)
{
    return pte_new(
               paddr >> seL4_PageBits,
               0, /* sw */
               1, /* dirty */
               1, /* accessed */
               0, /* global */
               RISCVGetUserFromVMRights(vm_rights),   /* user */
               executable, /* execute */
               RISCVGetWriteFromVMRights(vm_rights),  /* write */
               RISCVGetReadFromVMRights(vm_rights), /* read */
               1 /* valid */
           );
}

static inline bool_t CONST
checkVPAlignment(vm_page_size_t sz, word_t w)
{
    return (w & MASK(pageBitsForSize(sz))) == 0;
}

static exception_t
decodeRISCVPageTableInvocation(word_t label, unsigned int length,
                               cte_t *cte, cap_t cap, extra_caps_t extraCaps,
                               word_t *buffer)
{
    if (label == RISCVPageTableUnmap) {
        if (unlikely(!isFinalCapability(cte))) {
            userError("RISCVPageTableUnmap: cannot unmap if more than once cap exists");
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageTableInvocationUnmap (cap, cte);
    }

    if (unlikely((label != RISCVPageTableMap))) {
        userError("RISCVPageTable: Illegal Operation");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(length < 2 || extraCaps.excaprefs[0] == NULL)) {
        userError("RISCVPageTable: truncated message");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cap_page_table_cap_get_capPTMappedASID(cap) != asidInvalid)) {
        userError("RISCVPageTable: PageTable is already mapped.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    word_t vaddr = getSyscallArg(0, buffer);
    cap_t lvl1ptCap = extraCaps.excaprefs[0]->cap;

    if (unlikely(cap_get_capType(lvl1ptCap) != cap_page_table_cap && cap_page_table_cap_get_capPTIsMapped(lvl1ptCap))) {
        userError("RISCVPageTableMap: Invalid top-level PageTable.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pte_t *lvl1pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(lvl1ptCap));
    asid_t asid = cap_page_table_cap_get_capPTMappedASID(lvl1ptCap);

    if (unlikely(vaddr >= PPTR_USER_TOP)) {
        userError("RISCVPageTableMap: Virtual address cannot be in kernel window.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
    if (unlikely(find_ret.status != EXCEPTION_NONE)) {
        userError("RISCVPageTableMap: ASID lookup failed");
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(find_ret.vspace_root != lvl1pt)) {
        userError("RISCVPageTableMap: ASID lookup failed");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    lookupPTSlot_ret_t lu_ret = lookupPTSlot(lvl1pt, vaddr);

    /* if there is already something mapped (valid is set) or we have traversed far enough
     * that a page table is not valid to map then tell the user that they ahve to delete
     * something before they can put a PT here */
    if (lu_ret.ptBitsLeft == seL4_PageBits || pte_ptr_get_valid(lu_ret.ptSlot)) {
        userError("RISCVPageTableMap: All objects mapped at this address");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Get the slot to install the PT in */
    pte_t *ptSlot = lu_ret.ptSlot;

    paddr_t paddr = addrFromPPtr(
                        PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
    pte_t pte = pte_new((paddr >> seL4_PageBits),
                        0, /* sw */
                        1, /* dirty */
                        1, /* accessed */
                        0,  /* global */
                        0,  /* user */
                        0,  /* execute */
                        0,  /* write */
                        0,  /* read */
                        1 /* valid */
                       );

    cap = cap_page_table_cap_set_capPTIsMapped(cap, 1);
    cap = cap_page_table_cap_set_capPTMappedASID(cap, asid);
    cap = cap_page_table_cap_set_capPTMappedAddress(cap, vaddr);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performPageTableInvocationMap(cap, cte, pte, ptSlot);
}

static exception_t
decodeRISCVFrameInvocation(word_t label, unsigned int length,
                           cte_t *cte, cap_t cap, extra_caps_t extraCaps,
                           word_t *buffer)
{
    switch (label) {
    case RISCVPageMap: {
        if (unlikely(length < 3 || extraCaps.excaprefs[0] == NULL)) {
            userError("RISCVPageMap: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        word_t vaddr = getSyscallArg(0, buffer);
        word_t w_rightsMask = getSyscallArg(1, buffer);
        vm_attributes_t attr = vmAttributesFromWord(getSyscallArg(2, buffer));
        cap_t lvl1ptCap = extraCaps.excaprefs[0]->cap;

        vm_page_size_t frameSize = cap_frame_cap_get_capFSize(cap);
        vm_rights_t capVMRights = cap_frame_cap_get_capFVMRights(cap);

        /* check the frame isn't already mapped */
        if (unlikely(cap_frame_cap_get_capFMappedASID(cap)) != asidInvalid) {
            userError("RISCVPageMap: frame already mapped");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(cap_get_capType(lvl1ptCap) != cap_page_table_cap &&
                     cap_page_table_cap_get_capPTMappedASID(lvl1ptCap) == asidInvalid)) {
            userError("RISCVPageMap: Bad PageTable cap.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        pte_t *lvl1pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(lvl1ptCap));
        asid_t asid = cap_page_table_cap_get_capPTMappedASID(lvl1ptCap);

        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(asid);
        if (unlikely(find_ret.status != EXCEPTION_NONE)) {
            userError("RISCVPageMap: No PageTable for ASID");
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(find_ret.vspace_root != lvl1pt)) {
            userError("RISCVPageMap: ASID lookup failed");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* check the vaddr is valid */
        word_t vtop = vaddr + BIT(pageBitsForSize(frameSize)) - 1;
        if (unlikely(vtop >= PPTR_USER_TOP)) {
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if (unlikely(!checkVPAlignment(frameSize, vaddr))) {
            current_syscall_error.type = seL4_AlignmentError;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Check if this page is already mapped */
        lookupPTSlot_ret_t lu_ret = lookupPTSlot(lvl1pt, vaddr);
        if (unlikely(lu_ret.ptBitsLeft != pageBitsForSize(frameSize))) {
            current_lookup_fault = lookup_fault_missing_capability_new(lu_ret.ptBitsLeft);
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* check this vaddr isn't already mapped */
        if (unlikely(pte_ptr_get_valid(lu_ret.ptSlot))) {
            userError("Virtual address already mapped");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }

        vm_rights_t vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));
        paddr_t frame_paddr = addrFromPPtr((void *) cap_frame_cap_get_capFBasePtr(cap));
        cap = cap_frame_cap_set_capFMappedASID(cap, asid);
        cap = cap_frame_cap_set_capFMappedAddress(cap,  vaddr);

        bool_t executable = !vm_attributes_get_riscvExecuteNever(attr);
        pte_t pte = makeUserPTE(frame_paddr, executable, vmRights);
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageInvocationMapPTE(cap, cte, pte, lu_ret.ptSlot);
    }

    case RISCVPageRemap: {
        if (unlikely(length < 2 || extraCaps.excaprefs[0] == NULL)) {
            userError("RISCVPageRemap: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        word_t w_rightsMask = getSyscallArg(0, buffer);
        vm_attributes_t attr = vmAttributesFromWord(getSyscallArg(1, buffer));
        cap_t lvl1ptCap = extraCaps.excaprefs[0]->cap;
        vm_page_size_t frameSize = cap_frame_cap_get_capFSize(cap);
        vm_rights_t capVMRights = cap_frame_cap_get_capFVMRights(cap);

        if (unlikely(cap_get_capType(lvl1ptCap) != cap_page_table_cap &&
                     cap_page_table_cap_get_capPTMappedASID(lvl1ptCap) == asidInvalid)) {
            userError("RISCVPageRemap: Bad PageTable cap.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(cap_frame_cap_get_capFMappedASID(cap)) == asidInvalid) {
            userError("RISCVPageRemap: frame is not mapped");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        asid_t mappedASID = cap_frame_cap_get_capFMappedASID(cap);
        findVSpaceForASID_ret_t find_ret = findVSpaceForASID(mappedASID);
        if (unlikely(find_ret.status != EXCEPTION_NONE)) {
            userError("RISCVPageRemap: No PageTable for ASID");
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        pte_t *lvl1pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(lvl1ptCap));
        if (unlikely(find_ret.vspace_root != lvl1pt ||
                     cap_page_table_cap_get_capPTMappedASID(lvl1ptCap) != mappedASID)) {
            userError("RISCVPageRemap: ASID lookup failed");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;
            return EXCEPTION_SYSCALL_ERROR;
        }

        word_t vaddr = cap_frame_cap_get_capFMappedAddress(cap);
        if (unlikely(!checkVPAlignment(frameSize, vaddr))) {
            current_syscall_error.type = seL4_AlignmentError;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Check if this page is already mapped */
        lookupPTSlot_ret_t lu_ret = lookupPTSlot(lvl1pt, vaddr);

        if (unlikely(lu_ret.ptBitsLeft != pageBitsForSize(frameSize))) {
            userError("RISCVPageRemap: No PageTable for this page %p", (void*)vaddr);
            current_lookup_fault = lookup_fault_missing_capability_new(lu_ret.ptBitsLeft);
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (unlikely(isPTEPageTable(lu_ret.ptSlot))) {
            userError("RISCVPageRemap: no mapping to remap.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        vm_rights_t vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));
        paddr_t frame_paddr = addrFromPPtr((void *) cap_frame_cap_get_capFBasePtr(cap));
        bool_t executable = !vm_attributes_get_riscvExecuteNever(attr);
        pte_t pte = makeUserPTE(frame_paddr, executable, vmRights);
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageInvocationRemapPTE(pte, lu_ret.ptSlot);
    }
    case RISCVPageUnmap: {
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageInvocationUnmap(cap, cte);
    }

    case RISCVPageGetAddress: {

        /* Check that there are enough message registers */
        assert(n_msgRegisters >= 1);

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performPageGetAddress((void*)cap_frame_cap_get_capFBasePtr(cap));
    }

    default:
        userError("RISCVPage: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;

        return EXCEPTION_SYSCALL_ERROR;
    }

}

static const resolve_ret_t default_resolve_ret_t;

static inline vptr_t
pageBase(vptr_t vaddr, vm_page_size_t size)
{
    return vaddr & ~MASK(pageBitsForSize(size));
}

exception_t
decodeRISCVMMUInvocation(word_t label, unsigned int length, cptr_t cptr,
                         cte_t *cte, cap_t cap, extra_caps_t extraCaps,
                         word_t *buffer)
{
    switch (cap_get_capType(cap)) {

    case cap_page_table_cap:
        return decodeRISCVPageTableInvocation(label, length, cte, cap, extraCaps, buffer);

    case cap_frame_cap:
        return decodeRISCVFrameInvocation(label, length, cte, cap, extraCaps, buffer);

    case cap_asid_control_cap: {
        word_t     i;
        asid_t           asid_base;
        word_t           index;
        word_t           depth;
        cap_t            untyped;
        cap_t            root;
        cte_t*           parentSlot;
        cte_t*           destSlot;
        lookupSlot_ret_t lu_ret;
        void*            frame;
        exception_t      status;

        if (label != RISCVASIDControlMakePool) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (length < 2 || extraCaps.excaprefs[0] == NULL
                || extraCaps.excaprefs[1] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        index = getSyscallArg(0, buffer);
        depth = getSyscallArg(1, buffer);
        parentSlot = extraCaps.excaprefs[0];
        untyped = parentSlot->cap;
        root = extraCaps.excaprefs[1]->cap;

        /* Find first free pool */
        for (i = 0; i < nASIDPools && riscvKSASIDTable[i]; i++);

        if (i == nASIDPools) {
            /* no unallocated pool is found */
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid_base = i << asidLowBits;

        if (cap_get_capType(untyped) != cap_untyped_cap ||
                cap_untyped_cap_get_capBlockSize(untyped) != seL4_ASIDPoolBits ||
                cap_untyped_cap_get_capIsDevice(untyped)) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        status = ensureNoChildren(parentSlot);
        if (status != EXCEPTION_NONE) {
            return status;
        }

        frame = WORD_PTR(cap_untyped_cap_get_capPtr(untyped));

        lu_ret = lookupTargetSlot(root, index, depth);
        if (lu_ret.status != EXCEPTION_NONE) {
            return lu_ret.status;
        }
        destSlot = lu_ret.slot;

        status = ensureEmptySlot(destSlot);
        if (status != EXCEPTION_NONE) {
            return status;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performASIDControlInvocation(frame, destSlot, parentSlot, asid_base);
    }

    case cap_asid_pool_cap: {
        cap_t        vspaceCap;
        cte_t*       vspaceCapSlot;
        asid_pool_t* pool;
        word_t i;
        asid_t       asid;

        if (label != RISCVASIDPoolAssign) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }
        if (extraCaps.excaprefs[0] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vspaceCapSlot = extraCaps.excaprefs[0];
        vspaceCap = vspaceCapSlot->cap;

        if (cap_page_table_cap_get_capPTMappedASID(vspaceCap) != asidInvalid) {
            userError("RISCVASIDPool: Invalid vspace root.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        pool = riscvKSASIDTable[cap_asid_pool_cap_get_capASIDBase(cap) >> asidLowBits];
        if (!pool) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            current_lookup_fault = lookup_fault_invalid_root_new();
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (pool != ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Find first free ASID */
        asid = cap_asid_pool_cap_get_capASIDBase(cap);
        for (i = 0; i < BIT(asidLowBits) && (asid + i == 0 || pool->array[i]); i++);

        if (i == BIT(asidLowBits)) {
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid += i;

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performASIDPoolInvocation(asid, pool, vspaceCapSlot);
    }
    default:
        fail("Invalid arch cap type");
    }
}

exception_t
performPageTableInvocationMap(cap_t cap, cte_t *ctSlot,
                              pte_t pte, pte_t *ptSlot)
{
    ctSlot->cap = cap;
    *ptSlot = pte;

    return EXCEPTION_NONE;
}

exception_t
performPageTableInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    if (cap_page_table_cap_get_capPTIsMapped(cap)) {
        pte_t *pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap));
        unmapPageTable(
            cap_page_table_cap_get_capPTMappedASID(cap),
            cap_page_table_cap_get_capPTMappedAddress(cap),
            pt
        );
        clearMemory((void *)pt, seL4_PageBits);
    }
    cap_page_table_cap_ptr_set_capPTIsMapped(&(ctSlot->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t
performPageGetAddress(void *vbase_ptr)
{
    paddr_t capFBasePtr;

    /* Get the physical address of this frame. */
    capFBasePtr = addrFromPPtr(vbase_ptr);

    /* return it in the first message register */
    setRegister(NODE_STATE(ksCurThread), msgRegisters[0], capFBasePtr);
    setRegister(NODE_STATE(ksCurThread), msgInfoRegister,
                wordFromMessageInfo(seL4_MessageInfo_new(0, 0, 0, 1)));

    return EXCEPTION_NONE;
}

static exception_t updatePTE(pte_t pte, pte_t *base)
{
    *base = pte;
    sfence();
    return EXCEPTION_NONE;
}

exception_t performPageInvocationMapPTE(cap_t cap, cte_t *ctSlot,
                                        pte_t pte, pte_t *base)
{
    ctSlot->cap = cap;
    return updatePTE(pte, base);
}

exception_t
performPageInvocationRemapPTE(pte_t pte, pte_t *base)
{
    return updatePTE(pte, base);
}

exception_t
performPageInvocationUnmap(cap_t cap, cte_t *ctSlot)
{

    if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        unmapPage(cap_frame_cap_get_capFSize(cap),
                  cap_frame_cap_get_capFMappedASID(cap),
                  cap_frame_cap_get_capFMappedAddress(cap),
                  cap_frame_cap_get_capFBasePtr(cap)
                 );
    }
    ctSlot->cap = cap_frame_cap_set_capFMappedAddress(ctSlot->cap, 0);
    ctSlot->cap = cap_frame_cap_set_capFMappedASID(ctSlot->cap, asidInvalid);
    return EXCEPTION_NONE;
}

#ifdef CONFIG_PRINTING
void
Arch_userStackTrace(tcb_t *tptr)
{
    cap_t threadRoot = TCB_PTR_CTE_PTR(tptr, tcbVTable)->cap;
    if (!isValidVTableRoot(threadRoot)) {
        printf("Invalid vspace\n");
        return;
    }

    word_t sp = getRegister(tptr, SP);
    if (!IS_ALIGNED(sp, seL4_WordSizeBits)) {
        printf("SP %p not aligned", (void *) sp);
        return;
    }

    pte_t *vspace_root = PTE_PTR(pptr_of_cap(threadRoot));
    for (int i = 0; i < CONFIG_USER_STACK_TRACE_LENGTH; i++) {
        word_t address = sp + (i * sizeof(word_t));
        lookupPTSlot_ret_t ret = lookupPTSlot(vspace_root, address);
        if (pte_ptr_get_valid(ret.ptSlot) && !isPTEPageTable(ret.ptSlot)) {
            pptr_t pptr = (pptr_t) (getPPtrFromHWPTE(ret.ptSlot));
            word_t *value = (word_t*) ((word_t)pptr + (address & MASK(ret.ptBitsLeft)));
            printf("0x%lx: 0x%lx\n", (long) address, (long) *value);
        } else {
            printf("0x%lx: INVALID\n", (long) address);
        }
    }
}
#endif
