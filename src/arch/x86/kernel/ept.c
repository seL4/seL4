/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_VTX

#include <model/statedata.h>
#include <arch/kernel/ept.h>
#include <arch/api/invocation.h>

struct lookupEPTPDPTSlot_ret {
    exception_t status;
    ept_pdpte_t *pdptSlot;
};
typedef struct lookupEPTPDPTSlot_ret lookupEPTPDPTSlot_ret_t;

struct lookupEPTPDSlot_ret {
    exception_t status;
    ept_pde_t  *pdSlot;
};
typedef struct lookupEPTPDSlot_ret lookupEPTPDSlot_ret_t;

struct lookupEPTPTSlot_ret {
    exception_t status;
    ept_pte_t  *ptSlot;
};
typedef struct lookupEPTPTSlot_ret lookupEPTPTSlot_ret_t;

enum ept_cache_options {
    EPTUncacheable = 0,
    EPTWriteCombining = 1,
    EPTWriteThrough = 4,
    EPTWriteProtected = 5,
    EPTWriteBack = 6
};
typedef enum ept_cache_options ept_cache_options_t;

void deleteEPTASID(asid_t asid, ept_pml4e_t *ept)
{
    asid_pool_t *poolPtr;

    poolPtr = x86KSASIDTable[asid >> asidLowBits];
    if (poolPtr != NULL) {
        asid_map_t asid_map = poolPtr->array[asid & MASK(asidLowBits)];
        if (asid_map_get_type(asid_map) == asid_map_asid_map_ept &&
            (ept_pml4e_t *)asid_map_asid_map_ept_get_ept_root(asid_map) == ept) {
            poolPtr->array[asid & MASK(asidLowBits)] = asid_map_asid_map_none_new();
        }
    }
}

exception_t performX86EPTPageInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    unmapEPTPage(
        cap_frame_cap_get_capFSize(cap),
        cap_frame_cap_get_capFMappedASID(cap),
        cap_frame_cap_get_capFMappedAddress(cap),
        (void *)cap_frame_cap_get_capFBasePtr(cap)
    );

    cap_frame_cap_ptr_set_capFMappedAddress(&ctSlot->cap, 0);
    cap_frame_cap_ptr_set_capFMappedASID(&ctSlot->cap, asidInvalid);
    cap_frame_cap_ptr_set_capFMapType(&ctSlot->cap, X86_MappingNone);

    return EXCEPTION_NONE;
}

findEPTForASID_ret_t findEPTForASID(asid_t asid)
{
    findEPTForASID_ret_t ret;
    asid_map_t asid_map;

    asid_map = findMapForASID(asid);
    if (asid_map_get_type(asid_map) != asid_map_asid_map_ept) {
        current_lookup_fault = lookup_fault_invalid_root_new();

        ret.ept = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ret.ept = (ept_pml4e_t *)asid_map_asid_map_ept_get_ept_root(asid_map);
    ret.status = EXCEPTION_NONE;
    return ret;
}

static ept_pml4e_t *CONST lookupEPTPML4Slot(ept_pml4e_t *pml4, vptr_t vptr)
{
    return pml4 + GET_EPT_PML4_INDEX(vptr);
}

static lookupEPTPDPTSlot_ret_t CONST lookupEPTPDPTSlot(ept_pml4e_t *pml4, vptr_t vptr)
{
    lookupEPTPDPTSlot_ret_t ret;
    ept_pml4e_t *pml4Slot;

    pml4Slot = lookupEPTPML4Slot(pml4, vptr);

    if (!ept_pml4e_ptr_get_read(pml4Slot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(EPT_PML4_INDEX_OFFSET);

        ret.pdptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ept_pdpte_t *pdpt = paddr_to_pptr(ept_pml4e_ptr_get_pdpt_base_address(pml4Slot));
    uint32_t index = GET_EPT_PDPT_INDEX(vptr);
    ret.pdptSlot = pdpt + index;
    ret.status = EXCEPTION_NONE;
    return ret;
}

static lookupEPTPDSlot_ret_t lookupEPTPDSlot(ept_pml4e_t *pml4, vptr_t vptr)
{
    lookupEPTPDSlot_ret_t ret;
    lookupEPTPDPTSlot_ret_t lu_ret;

    lu_ret = lookupEPTPDPTSlot(pml4, vptr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        /* current_lookup_fault will have been set by lookupEPTPDPTSlot */
        ret.pdSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    if (!ept_pdpte_ptr_get_read(lu_ret.pdptSlot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(EPT_PDPT_INDEX_OFFSET);

        ret.pdSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ept_pde_t *pd = paddr_to_pptr(ept_pdpte_ptr_get_pd_base_address(lu_ret.pdptSlot));
    uint32_t index = GET_EPT_PD_INDEX(vptr);
    ret.pdSlot = pd + index;
    ret.status = EXCEPTION_NONE;
    return ret;
}

static lookupEPTPTSlot_ret_t lookupEPTPTSlot(ept_pml4e_t *pml4, vptr_t vptr)
{
    lookupEPTPTSlot_ret_t ret;
    lookupEPTPDSlot_ret_t lu_ret;

    lu_ret = lookupEPTPDSlot(pml4, vptr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        /* current_lookup_fault will have been set by lookupEPTPDSlot */
        ret.ptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    if ((ept_pde_ptr_get_page_size(lu_ret.pdSlot) != ept_pde_ept_pde_pt) ||
        !ept_pde_ept_pde_pt_ptr_get_read(lu_ret.pdSlot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(EPT_PD_INDEX_OFFSET);

        ret.ptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ept_pte_t *pt = paddr_to_pptr(ept_pde_ept_pde_pt_ptr_get_pt_base_address(lu_ret.pdSlot));
    uint32_t index = GET_EPT_PT_INDEX(vptr);

    ret.ptSlot = pt + index;
    ret.status = EXCEPTION_NONE;
    return ret;
}

static ept_cache_options_t eptCacheFromVmAttr(vm_attributes_t vmAttr)
{
    /* Need to sanitise user input, vmAttr will not have been verified at this point. */
    ept_cache_options_t option = vmAttr.words[0];
    if (option != EPTUncacheable &&
        option != EPTWriteCombining &&
        option != EPTWriteThrough &&
        option != EPTWriteBack) {
        option = EPTWriteBack;
    }
    return option;
}

EPTPDPTMapped_ret_t EPTPDPTMapped(asid_t asid, vptr_t vptr, ept_pdpte_t *pdpt)
{
    EPTPDPTMapped_ret_t ret;
    findEPTForASID_ret_t asid_ret;
    ept_pml4e_t *pml4Slot;

    asid_ret = findEPTForASID(asid);
    if (asid_ret.status != EXCEPTION_NONE) {
        ret.pml4 = NULL;
        ret.pml4Slot = NULL;
        ret.status = asid_ret.status;
        return ret;
    }

    pml4Slot = lookupEPTPML4Slot(asid_ret.ept, vptr);

    if (ept_pml4e_ptr_get_read(pml4Slot)
        && ptrFromPAddr(ept_pml4e_ptr_get_pdpt_base_address(pml4Slot)) == pdpt) {
        ret.pml4 = asid_ret.ept;
        ret.pml4Slot = pml4Slot;
        ret.status = EXCEPTION_NONE;
        return ret;
    } else {
        ret.pml4 = NULL;
        ret.pml4Slot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }
}

void unmapEPTPDPT(asid_t asid, vptr_t vaddr, ept_pdpte_t *pdpt)
{
    EPTPDPTMapped_ret_t lu_ret;

    lu_ret = EPTPDPTMapped(asid, vaddr, pdpt);

    if (lu_ret.status == EXCEPTION_NONE) {
        *lu_ret.pml4Slot = ept_pml4e_new(0, 0, 0, 0);
        invept(lu_ret.pml4);
    }
}

static exception_t performEPTPDPTInvocationUnmap(cap_t cap, cte_t *cte)
{
    if (cap_ept_pdpt_cap_get_capPDPTIsMapped(cap)) {
        ept_pdpte_t *pdpt = (ept_pdpte_t *)cap_ept_pdpt_cap_get_capPDPTBasePtr(cap);
        unmapEPTPDPT(
            cap_ept_pdpt_cap_get_capPDPTMappedASID(cap),
            cap_ept_pdpt_cap_get_capPDPTMappedAddress(cap),
            pdpt);
        clearMemory((void *)pdpt, cap_get_capSizeBits(cap));
    }
    cap_ept_pdpt_cap_ptr_set_capPDPTIsMapped(&(cte->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t performEPTPDPTInvocationMap(cap_t cap, cte_t *cte, ept_pml4e_t pml4e, ept_pml4e_t *pml4Slot,
                                               ept_pml4e_t *pml4)
{
    cte->cap = cap;
    *pml4Slot = pml4e;
    invept(pml4);

    return EXCEPTION_NONE;
}

static exception_t decodeX86EPTPDPTInvocation(
    word_t invLabel,
    word_t length,
    cte_t *cte,
    cap_t cap,
    word_t *buffer
)
{
    word_t          vaddr;
    cap_t           pml4Cap;
    ept_pml4e_t    *pml4;
    ept_pml4e_t     pml4e;
    paddr_t         paddr;
    asid_t          asid;
    findEPTForASID_ret_t find_ret;
    ept_pml4e_t    *pml4Slot;

    if (invLabel == X86EPTPDPTUnmap) {
        if (!isFinalCapability(cte)) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performEPTPDPTInvocationUnmap(cap, cte);
    }

    if (invLabel != X86EPTPDPTMap) {
        userError("X86EPTPDPT Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || current_extra_caps.excaprefs[0] == NULL) {
        userError("X86EPTPDPTMap: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_ept_pdpt_cap_get_capPDPTIsMapped(cap)) {
        userError("X86EPTPDPTMap: EPT PDPT is already mapped to a PML4.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
    /* cannot use ~MASK(EPT_PML4_INDEX_OFFSET) because on 32-bit compilations
     * this results in an error shifting by greater than 31 bits, so we manually
     * force a 64-bit variable to do the shifting with */
    vaddr = vaddr & ~(((uint64_t)1 << EPT_PML4_INDEX_OFFSET) - 1);
    pml4Cap = current_extra_caps.excaprefs[0]->cap;

    if (cap_get_capType(pml4Cap) != cap_ept_pml4_cap) {
        userError("X86EPTPDPTMap: Not a valid EPT PML4.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pml4 = (ept_pml4e_t *)cap_ept_pml4_cap_get_capPML4BasePtr(pml4Cap);
    asid = cap_ept_pml4_cap_get_capPML4MappedASID(pml4Cap);

    find_ret = findEPTForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (find_ret.ept != pml4) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pml4Slot = lookupEPTPML4Slot(pml4, vaddr);

    if (ept_pml4e_ptr_get_read(pml4Slot)) {
        userError("X86EPTPDPTMap: PDPT already mapped here.");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void *)cap_ept_pdpt_cap_get_capPDPTBasePtr(cap));
    pml4e = ept_pml4e_new(
                paddr,
                1,
                1,
                1
            );

    cap = cap_ept_pdpt_cap_set_capPDPTIsMapped(cap, 1);
    cap = cap_ept_pdpt_cap_set_capPDPTMappedASID(cap, asid);
    cap = cap_ept_pdpt_cap_set_capPDPTMappedAddress(cap, vaddr);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performEPTPDPTInvocationMap(cap, cte, pml4e, pml4Slot, pml4);
}

exception_t decodeX86EPTInvocation(
    word_t invLabel,
    word_t length,
    cptr_t cptr,
    cte_t *cte,
    cap_t cap,
    word_t *buffer
)
{
    switch (cap_get_capType(cap)) {
    case cap_ept_pdpt_cap:
        return decodeX86EPTPDPTInvocation(invLabel, length, cte, cap, buffer);
    case cap_ept_pd_cap:
        return decodeX86EPTPDInvocation(invLabel, length, cte, cap, buffer);
    case cap_ept_pt_cap:
        return decodeX86EPTPTInvocation(invLabel, length, cte, cap, buffer);
    default:
        fail("Invalid cap type");
    }
}

EPTPageDirectoryMapped_ret_t EPTPageDirectoryMapped(asid_t asid, vptr_t vaddr, ept_pde_t *pd)
{
    EPTPageDirectoryMapped_ret_t ret;
    lookupEPTPDPTSlot_ret_t find_ret;
    findEPTForASID_ret_t asid_ret;

    asid_ret = findEPTForASID(asid);
    if (asid_ret.status != EXCEPTION_NONE) {
        ret.pml4 = NULL;
        ret.pdptSlot = NULL;
        ret.status = asid_ret.status;
        return ret;
    }

    find_ret = lookupEPTPDPTSlot(asid_ret.ept, vaddr);
    if (find_ret.status != EXCEPTION_NONE) {
        ret.pml4 = NULL;
        ret.pdptSlot = NULL;
        ret.status = find_ret.status;
        return ret;
    }

    if (ept_pdpte_ptr_get_read(find_ret.pdptSlot)
        && ptrFromPAddr(ept_pdpte_ptr_get_pd_base_address(find_ret.pdptSlot)) == pd) {
        ret.pml4 = asid_ret.ept;
        ret.pdptSlot = find_ret.pdptSlot;
        ret.status = EXCEPTION_NONE;
        return ret;
    } else {
        ret.pml4 = NULL;
        ret.pdptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }
}

void unmapEPTPageDirectory(asid_t asid, vptr_t vaddr, ept_pde_t *pd)
{
    EPTPageDirectoryMapped_ret_t lu_ret;

    lu_ret = EPTPageDirectoryMapped(asid, vaddr, pd);

    if (lu_ret.status == EXCEPTION_NONE) {
        *lu_ret.pdptSlot = ept_pdpte_new(
                               0,  /* pd_base_address  */
                               0,  /* avl_cte_depth    */
                               0,  /* execute          */
                               0,  /* write            */
                               0   /* read             */
                           );
        invept(lu_ret.pml4);
    }
}

static exception_t performEPTPDInvocationUnmap(cap_t cap, cte_t *cte)
{
    if (cap_ept_pd_cap_get_capPDIsMapped(cap)) {
        ept_pde_t *pd = (ept_pde_t *)cap_ept_pd_cap_get_capPDBasePtr(cap);
        unmapEPTPageDirectory(
            cap_ept_pd_cap_get_capPDMappedASID(cap),
            cap_ept_pd_cap_get_capPDMappedAddress(cap),
            pd);
        clearMemory((void *)pd, cap_get_capSizeBits(cap));
    }
    cap_ept_pd_cap_ptr_set_capPDIsMapped(&(cte->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t performEPTPDInvocationMap(cap_t cap, cte_t *cte, ept_pdpte_t pdpte, ept_pdpte_t *pdptSlot,
                                             ept_pml4e_t *pml4)
{
    cte->cap = cap;
    *pdptSlot = pdpte;
    invept(pml4);

    return EXCEPTION_NONE;
}

exception_t decodeX86EPTPDInvocation(
    word_t invLabel,
    word_t length,
    cte_t *cte,
    cap_t cap,
    word_t *buffer
)
{
    word_t          vaddr;
    cap_t           pml4Cap;
    ept_pml4e_t    *pml4;
    ept_pdpte_t     pdpte;
    paddr_t         paddr;
    asid_t          asid;
    findEPTForASID_ret_t find_ret;
    lookupEPTPDPTSlot_ret_t lu_ret;

    if (invLabel == X86EPTPDUnmap) {
        if (!isFinalCapability(cte)) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performEPTPDInvocationUnmap(cap, cte);
    }

    if (invLabel != X86EPTPDMap) {
        userError("X86EPTPD Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || current_extra_caps.excaprefs[0] == NULL) {
        userError("X86EPTPDMap: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_ept_pd_cap_get_capPDIsMapped(cap)) {
        userError("X86EPTPDMap: EPT Page directory is already mapped to a PDPT.");
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
    vaddr = vaddr & ~MASK(EPT_PDPT_INDEX_OFFSET);
    pml4Cap = current_extra_caps.excaprefs[0]->cap;

    if (cap_get_capType(pml4Cap) != cap_ept_pml4_cap) {
        userError("X86EPTPDMap: Not a valid EPT pml4.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pml4 = (ept_pml4e_t *)cap_ept_pml4_cap_get_capPML4BasePtr(pml4Cap);
    asid = cap_ept_pml4_cap_get_capPML4MappedASID(pml4Cap);

    find_ret = findEPTForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        userError("X86EPTPDMap: EPT PML4 is not mapped.");
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (find_ret.ept != pml4) {
        userError("X86EPTPDMap: EPT PML4 asid is invalid.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    lu_ret = lookupEPTPDPTSlot(pml4, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (ept_pdpte_ptr_get_read(lu_ret.pdptSlot)) {
        userError("X86EPTPDMap: Page directory already mapped here.");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void *)(cap_ept_pd_cap_get_capPDBasePtr(cap)));
    pdpte = ept_pdpte_new(
                paddr,  /* pd_base_address  */
                0,      /* avl_cte_depth    */
                1,      /* execute          */
                1,      /* write            */
                1       /* read             */
            );

    cap = cap_ept_pd_cap_set_capPDIsMapped(cap, 1);
    cap = cap_ept_pd_cap_set_capPDMappedASID(cap, asid);
    cap = cap_ept_pd_cap_set_capPDMappedAddress(cap, vaddr);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performEPTPDInvocationMap(cap, cte, pdpte, lu_ret.pdptSlot, pml4);
}

EPTPageTableMapped_ret_t EPTPageTableMapped(asid_t asid, vptr_t vaddr, ept_pte_t *pt)
{
    EPTPageTableMapped_ret_t ret;
    lookupEPTPDSlot_ret_t find_ret;
    findEPTForASID_ret_t asid_ret;

    asid_ret = findEPTForASID(asid);
    if (asid_ret.status != EXCEPTION_NONE) {
        ret.pml4 = NULL;
        ret.pdSlot = NULL;
        ret.status = asid_ret.status;
        return ret;
    }

    find_ret = lookupEPTPDSlot(asid_ret.ept, vaddr);
    if (find_ret.status != EXCEPTION_NONE) {
        ret.pml4 = NULL;
        ret.pdSlot = NULL;
        ret.status = find_ret.status;
        return ret;
    }

    if (ept_pde_ptr_get_page_size(find_ret.pdSlot) == ept_pde_ept_pde_pt
        && ptrFromPAddr(ept_pde_ept_pde_pt_ptr_get_pt_base_address(find_ret.pdSlot)) == pt) {
        ret.pml4 = asid_ret.ept;
        ret.pdSlot = find_ret.pdSlot;
        ret.status = EXCEPTION_NONE;
        return ret;
    } else {
        ret.pml4 = NULL;
        ret.pdSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }
}

void unmapEPTPageTable(asid_t asid, vptr_t vaddr, ept_pte_t *pt)
{
    EPTPageTableMapped_ret_t lu_ret;

    lu_ret = EPTPageTableMapped(asid, vaddr, pt);

    if (lu_ret.status == EXCEPTION_NONE) {
        *lu_ret.pdSlot = ept_pde_ept_pde_pt_new(
                             0,  /* pt_base_address  */
                             0,  /* avl_cte_depth    */
                             0,  /* execute          */
                             0,  /* write            */
                             0   /* read             */
                         );
        invept(lu_ret.pml4);
    }
}

static exception_t performEPTPTInvocationUnmap(cap_t cap, cte_t *cte)
{
    if (cap_ept_pt_cap_get_capPTIsMapped(cap)) {
        ept_pte_t *pt = (ept_pte_t *)cap_ept_pt_cap_get_capPTBasePtr(cap);
        unmapEPTPageTable(
            cap_ept_pt_cap_get_capPTMappedASID(cap),
            cap_ept_pt_cap_get_capPTMappedAddress(cap),
            pt);
        clearMemory((void *)pt, cap_get_capSizeBits(cap));
    }
    cap_ept_pt_cap_ptr_set_capPTIsMapped(&(cte->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t performEPTPTInvocationMap(cap_t cap, cte_t *cte, ept_pde_t pde, ept_pde_t *pdSlot, ept_pml4e_t *pml4)
{
    cte->cap = cap;
    *pdSlot = pde;
    invept(pml4);

    return EXCEPTION_NONE;
}

exception_t decodeX86EPTPTInvocation(
    word_t invLabel,
    word_t length,
    cte_t *cte,
    cap_t cap,
    word_t *buffer
)
{
    word_t          vaddr;
    cap_t           pml4Cap;
    ept_pml4e_t    *pml4;
    ept_pde_t       pde;
    paddr_t         paddr;
    asid_t          asid;
    findEPTForASID_ret_t find_ret;
    lookupEPTPDSlot_ret_t lu_ret;

    if (invLabel == X86EPTPTUnmap) {
        if (!isFinalCapability(cte)) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performEPTPTInvocationUnmap(cap, cte);
    }

    if (invLabel != X86EPTPTMap) {
        userError("X86EPTPT Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || current_extra_caps.excaprefs[0] == NULL) {
        userError("X86EPTPT: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_ept_pt_cap_get_capPTIsMapped(cap)) {
        userError("X86EPTPT EPT Page table is already mapped to an EPT page directory.");
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
    vaddr = vaddr & ~MASK(EPT_PD_INDEX_OFFSET);
    pml4Cap = current_extra_caps.excaprefs[0]->cap;

    if (cap_get_capType(pml4Cap) != cap_ept_pml4_cap ||
        !cap_ept_pml4_cap_get_capPML4IsMapped(pml4Cap)) {
        userError("X86EPTPTMap: Not a valid EPT pml4.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pml4 = (ept_pml4e_t *)(cap_ept_pml4_cap_get_capPML4BasePtr(pml4Cap));
    asid = cap_ept_pml4_cap_get_capPML4MappedASID(pml4Cap);

    find_ret = findEPTForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (find_ret.ept != pml4) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    lu_ret = lookupEPTPDSlot(pml4, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        /* current_lookup_fault will have been set by lookupPTSlot */
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (((ept_pde_ptr_get_page_size(lu_ret.pdSlot) == ept_pde_ept_pde_pt) &&
         ept_pde_ept_pde_pt_ptr_get_read(lu_ret.pdSlot)) ||
        ((ept_pde_ptr_get_page_size(lu_ret.pdSlot) == ept_pde_ept_pde_2m) &&
         ept_pde_ept_pde_2m_ptr_get_read(lu_ret.pdSlot))) {
        userError("X86EPTPTMap: Page table already mapped here");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void *)(cap_ept_pt_cap_get_capPTBasePtr(cap)));
    pde = ept_pde_ept_pde_pt_new(
              paddr,/* pt_base_address  */
              0,    /* avl_cte_depth    */
              1,    /* execute          */
              1,    /* write            */
              1     /* read             */
          );

    cap = cap_ept_pt_cap_set_capPTIsMapped(cap, 1);
    cap = cap_ept_pt_cap_set_capPTMappedASID(cap, asid);
    cap = cap_ept_pt_cap_set_capPTMappedAddress(cap, vaddr);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performEPTPTInvocationMap(cap, cte, pde, lu_ret.pdSlot, pml4);
}

static exception_t performEPTPageMapPTE(cap_t cap, cte_t *cte, ept_pte_t *ptSlot, ept_pte_t pte, ept_pml4e_t *pml4)
{
    *ptSlot = pte;
    cte->cap = cap;
    invept(pml4);

    return EXCEPTION_NONE;
}

static exception_t performEPTPageMapPDE(cap_t cap, cte_t *cte, ept_pde_t *pdSlot, ept_pde_t pde1, ept_pde_t pde2,
                                        ept_pml4e_t *pml4)
{
    pdSlot[0] = pde1;
    if (LARGE_PAGE_BITS == 22) {
        pdSlot[1] = pde2;
    }
    cte->cap = cap;
    invept(pml4);

    return EXCEPTION_NONE;
}

exception_t decodeX86EPTPageMap(
    word_t invLabel,
    word_t length,
    cte_t *cte,
    cap_t cap,
    word_t *buffer)
{
    word_t          vaddr;
    word_t          w_rightsMask;
    paddr_t         paddr;
    cap_t           pml4Cap;
    ept_pml4e_t    *pml4;
    vm_rights_t     capVMRights;
    vm_rights_t     vmRights;
    vm_attributes_t vmAttr;
    vm_page_size_t  frameSize;
    asid_t          asid;

    frameSize = cap_frame_cap_get_capFSize(cap);
    vaddr = getSyscallArg(0, buffer);
    vaddr = vaddr & ~MASK(EPT_PT_INDEX_OFFSET);
    w_rightsMask = getSyscallArg(1, buffer);
    vmAttr = vmAttributesFromWord(getSyscallArg(2, buffer));
    pml4Cap = current_extra_caps.excaprefs[0]->cap;

    capVMRights = cap_frame_cap_get_capFVMRights(cap);

    if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        userError("X86EPTPageMap: Frame already mapped.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    assert(cap_frame_cap_get_capFMapType(cap) == X86_MappingNone);

    if (cap_get_capType(pml4Cap) != cap_ept_pml4_cap ||
        !cap_ept_pml4_cap_get_capPML4IsMapped(pml4Cap)) {
        userError("X86EPTPageMap: Attempting to map frame into invalid ept pml4.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pml4 = (ept_pml4e_t *)(cap_ept_pml4_cap_get_capPML4BasePtr(pml4Cap));
    asid = cap_ept_pml4_cap_get_capPML4MappedASID(pml4Cap);

    findEPTForASID_ret_t find_ret = findEPTForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (find_ret.ept != pml4) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }


    vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

    if (!checkVPAlignment(frameSize, vaddr)) {
        current_syscall_error.type = seL4_AlignmentError;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void *)cap_frame_cap_get_capFBasePtr(cap));

    cap = cap_frame_cap_set_capFMappedASID(cap, asid);
    cap = cap_frame_cap_set_capFMappedAddress(cap, vaddr);
    cap = cap_frame_cap_set_capFMapType(cap, X86_MappingEPT);

    switch (frameSize) {
    /* PTE mappings */
    case X86_SmallPage: {
        lookupEPTPTSlot_ret_t lu_ret;
        ept_pte_t pte;

        lu_ret = lookupEPTPTSlot(pml4, vaddr);
        if (lu_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            /* current_lookup_fault will have been set by lookupEPTPTSlot */
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (ept_pte_ptr_get_read(lu_ret.ptSlot)) {
            userError("X86EPTPageMap: Mapping already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }

        pte = ept_pte_new(
                  paddr,
                  0,
                  0,
                  eptCacheFromVmAttr(vmAttr),
                  1,
                  WritableFromVMRights(vmRights),
                  1);

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performEPTPageMapPTE(cap, cte, lu_ret.ptSlot, pte, pml4);
    }

    /* PDE mappings */
    case X86_LargePage: {
        lookupEPTPDSlot_ret_t lu_ret;

        lu_ret = lookupEPTPDSlot(pml4, vaddr);
        if (lu_ret.status != EXCEPTION_NONE) {
            userError("X86EPTPageMap: Need a page directory first.");
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            /* current_lookup_fault will have been set by lookupEPTPDSlot */
            return EXCEPTION_SYSCALL_ERROR;
        }


        if ((ept_pde_ptr_get_page_size(lu_ret.pdSlot) == ept_pde_ept_pde_pt) &&
            ept_pde_ept_pde_pt_ptr_get_read(lu_ret.pdSlot)) {
            userError("X86EPTPageMap: Page table already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if (LARGE_PAGE_BITS != EPT_PD_INDEX_OFFSET &&
            (ept_pde_ptr_get_page_size(lu_ret.pdSlot + 1) == ept_pde_ept_pde_pt) &&
            ept_pde_ept_pde_pt_ptr_get_read(lu_ret.pdSlot + 1)) {
            userError("X86EPTPageMap: Page table already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if ((ept_pde_ptr_get_page_size(lu_ret.pdSlot) == ept_pde_ept_pde_2m) &&
            ept_pde_ept_pde_2m_ptr_get_read(lu_ret.pdSlot)) {
            userError("X86EPTPageMap: Mapping already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }

        ept_pde_t pde1 = ept_pde_ept_pde_2m_new(
                             paddr,
                             0,
                             0,
                             eptCacheFromVmAttr(vmAttr),
                             1,
                             WritableFromVMRights(vmRights),
                             1);

        ept_pde_t pde2 = ept_pde_ept_pde_2m_new(
                             paddr + BIT(EPT_PD_INDEX_OFFSET),
                             0,
                             0,
                             eptCacheFromVmAttr(vmAttr),
                             1,
                             WritableFromVMRights(vmRights),
                             1);

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performEPTPageMapPDE(cap, cte, lu_ret.pdSlot, pde1, pde2, pml4);
    }

    default:
        /* When initializing EPT we only checked for support for 4K and 2M
         * pages, so we must disallow attempting to use any other */
        userError("X86EPTPageMap: Attempted to map unsupported page size.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

void unmapEPTPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, void *pptr)
{
    findEPTForASID_ret_t find_ret;
    paddr_t addr = addrFromPPtr(pptr);

    find_ret = findEPTForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    switch (page_size) {
    case X86_SmallPage: {
        lookupEPTPTSlot_ret_t lu_ret;

        lu_ret = lookupEPTPTSlot(find_ret.ept, vptr);
        if (lu_ret.status != EXCEPTION_NONE) {
            return;
        }
        if (!ept_pte_ptr_get_read(lu_ret.ptSlot)) {
            return;
        }
        if (ept_pte_ptr_get_page_base_address(lu_ret.ptSlot) != addr) {
            return;
        }

        *lu_ret.ptSlot = ept_pte_new(0, 0, 0, 0, 0, 0, 0);
        break;
    }
    case X86_LargePage: {
        lookupEPTPDSlot_ret_t lu_ret;

        lu_ret = lookupEPTPDSlot(find_ret.ept, vptr);
        if (lu_ret.status != EXCEPTION_NONE) {
            return;
        }
        if (ept_pde_ptr_get_page_size(lu_ret.pdSlot) != ept_pde_ept_pde_2m) {
            return;
        }
        if (!ept_pde_ept_pde_2m_ptr_get_read(lu_ret.pdSlot)) {
            return;
        }
        if (ept_pde_ept_pde_2m_ptr_get_page_base_address(lu_ret.pdSlot) != addr) {
            return;
        }

        lu_ret.pdSlot[0] = ept_pde_ept_pde_2m_new(0, 0, 0, 0, 0, 0, 0);

        if (LARGE_PAGE_BITS != EPT_PD_INDEX_OFFSET) {
            assert(ept_pde_ptr_get_page_size(lu_ret.pdSlot + 1) == ept_pde_ept_pde_2m);
            assert(ept_pde_ept_pde_2m_ptr_get_read(lu_ret.pdSlot + 1));
            assert(ept_pde_ept_pde_2m_ptr_get_page_base_address(lu_ret.pdSlot + 1) == addr + BIT(21));

            lu_ret.pdSlot[1] = ept_pde_ept_pde_2m_new(0, 0, 0, 0, 0, 0, 0);
        }
        break;
    }
    default:
        /* we did not allow mapping additional page sizes into EPT objects,
         * so this should not happen. As we have no way to return an error
         * all we can do is assert */
        assert(!"Invalid page size for unmap");
    }
}

#endif /* CONFIG_VTX */
