/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

#ifdef CONFIG_IOMMU

#include <api/syscall.h>
#include <machine/io.h>
#include <kernel/thread.h>
#include <arch/api/invocation.h>
#include <arch/object/iospace.h>
#include <arch/model/statedata.h>
#include <arch/linker.h>
#include <plat/machine/intel-vtd.h>

typedef struct lookupVTDContextSlot_ret {
    vtd_cte_t *cte;
    uint32_t index;
} lookupVTDContextSlot_ret_t;

BOOT_CODE cap_t
master_iospace_cap(void)
{
    if (ia32KSnumDrhu == 0) {
        return cap_null_cap_new();
    }

    return
        cap_io_space_cap_new(
            0,              /* capDomainID  */
            0              /* capPCIDevice */
        );
}

/* parent[index] IS child */
void unmapVTDPT(vtd_pte_t *parent, vtd_pte_t *child, uint32_t index)
{
    parent[index] = vtd_pte_new(0, 0, 0, 0);
    flushCacheRange(parent + index, VTD_PTE_SIZE_BITS);
    invalidate_iotlb();
}

void unmapAllIOPT(vtd_pte_t *pt, int level)
{
    uint32_t i;

    for (i = 0; i < BIT(VTD_PT_BITS); i++) {
        /* see if there is anything here */
        vtd_pte_t *pte = pt + i;
        if (vtd_pte_ptr_get_read(pte)) {
            uint32_t depth = vtd_pte_ptr_get_cte_depth(pte);
            /* work out if we are searching for frames or more page tables */
            if (level == ia32KSnumIOPTLevels - 1) {
                cap_t cap;
                void *frame = paddr_to_pptr(vtd_pte_ptr_get_addr(pte));
                cte_t *cte = cdtFindAtDepth(cap_frame_cap_new(IA32_SmallPage, VTD_PT_REF(pt), i, IA32_MAPPING_IO, 0, (uint32_t)frame), depth);
                assert(cte);
                cap = cap_frame_cap_set_capFMappedObject(cte->cap, 0);
                cdtUpdate(cte, cap);
            } else {
                cap_t cap;
                vtd_pte_t *pt2 = VTD_PTE_PTR(paddr_to_pptr(vtd_pte_ptr_get_addr(pte)));
                cte_t *cte = cdtFindAtDepth(cap_io_page_table_cap_new(0, VTD_PT_REF(pt), i, VTD_PT_REF(pt2)), depth);
                assert(cte);
                cap = cap_io_page_table_cap_set_capIOPTMappedObject(cte->cap, 0);
                cdtUpdate(cte, cap);
            }
        }
    }
}

static void unmapVTDContextEntryAt(vtd_cte_t *cte, uint32_t index)
{
    /* Lookup the page table and unmap it */
    vtd_pte_t *vtd_pt;
    cte_t *ptCte;
    cap_t ptCap;
    vtd_cte_t *vtd_context_slot = cte + index;
    /* First see if there is a page table */
    if (!vtd_cte_ptr_get_present(vtd_context_slot)) {
        return;
    }
    /* see if it reserved, and thus shouldn't be unmapped */
    if (vtd_cte_ptr_get_rmrr(vtd_context_slot)) {
        return;
    }
    /* Lookup the slot */
    vtd_pt = (vtd_pte_t*)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
    ptCte = cdtFindAtDepth(cap_io_page_table_cap_new(0, (uint32_t)cte, index, VTD_PT_REF(vtd_pt)), vtd_cte_ptr_get_cte_depth(vtd_context_slot));
    assert(ptCte);
    /* unmap */
    ptCap = cap_io_page_table_cap_set_capIOPTMappedObject(ptCte->cap, 0);
    cdtUpdate(ptCte, ptCap);
    assert(vtd_cte_ptr_get_present(vtd_context_slot));
    vtd_cte_ptr_new(
        vtd_context_slot,
        0,  /* Domain ID          */
        0,  /* CTE Depth          */
        0,  /* RMRR Mapping       */
        0,  /* Address Width      */
        0,  /* Address Space Root */
        0,  /* Translation Type   */
        0   /* Present            */
    );
    flushCacheRange(vtd_context_slot, VTD_CTE_SIZE_BITS);
    invalidate_iotlb();
}

static lookupVTDContextSlot_ret_t lookupVTDContextSlot_helper(cap_t cap)
{
    uint32_t   vtd_root_index;
    uint32_t   vtd_context_index;
    dev_id_t   pci_request_id;
    vtd_rte_t* vtd_root_slot;
    vtd_cte_t* vtd_context;

    assert(cap_get_capType(cap) == cap_io_space_cap);
    pci_request_id = cap_io_space_cap_get_capPCIDevice(cap);

    vtd_root_index = vtd_get_root_index(pci_request_id);
    vtd_root_slot = ia32KSvtdRootTable + vtd_root_index;

    vtd_context = (vtd_cte_t*)paddr_to_pptr(vtd_rte_ptr_get_ctp(vtd_root_slot));
    vtd_context_index = vtd_get_context_index(pci_request_id);

    return (lookupVTDContextSlot_ret_t) {
        .cte = vtd_context, .index = vtd_context_index
    };

}

vtd_cte_t *lookupVTDContextSlot(cap_t cap)
{
    lookupVTDContextSlot_ret_t ret = lookupVTDContextSlot_helper(cap);
    return ret.cte + ret.index;
}

void unmapVTDContextEntry(cap_t cap)
{
    lookupVTDContextSlot_ret_t ret = lookupVTDContextSlot_helper(cap);
    unmapVTDContextEntryAt(ret.cte, ret.index);
}

static lookupIOPTSlot_ret_t lookupIOPTSlot_helper(vtd_pte_t* iopt, word_t translation, word_t levels)
{
    lookupIOPTSlot_ret_t ret;
    uint32_t             iopt_index;
    vtd_pte_t*           vtd_pte_slot;
    vtd_pte_t*           vtd_next_level_iopt;

    if (VTD_PT_BITS * levels >= 32) {
        iopt_index = 0;
    } else {
        iopt_index = (translation >> (VTD_PT_BITS * levels)) & MASK(VTD_PT_BITS);
    }

    vtd_pte_slot = iopt + iopt_index;

    if (!vtd_pte_ptr_get_write(vtd_pte_slot) || levels == 0) {
        /* Slot is in this page table level */
        ret.iopt = iopt;
        ret.index = iopt_index;
        ret.level    = ia32KSnumIOPTLevels - levels;
        ret.status   = EXCEPTION_NONE;
        return ret;
    } else {
        vtd_next_level_iopt = (vtd_pte_t*)paddr_to_pptr(vtd_pte_ptr_get_addr(vtd_pte_slot));
        return lookupIOPTSlot_helper(vtd_next_level_iopt, translation, levels - 1);
    }
}

static inline lookupIOPTSlot_ret_t lookupIOPTSlot(vtd_pte_t* iopt, word_t io_address)
{
    lookupIOPTSlot_ret_t ret;

    if (iopt == 0) {
        ret.iopt        = 0;
        ret.index       = 0;
        ret.level       = 0;
        ret.status      = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        return lookupIOPTSlot_helper(iopt, io_address >> PAGE_BITS, ia32KSnumIOPTLevels - 1);
    }
}

void
unmapIOPTCap(cap_t cap)
{

    int level = cap_io_page_table_cap_get_capIOPTLevel(cap);
    vtd_pte_t *pt = VTD_PTE_PTR(cap_get_capPtr(cap));
    if (level == 0) {
        vtd_cte_t *ct = VTD_CTE_PTR(cap_io_page_table_cap_get_capIOPTMappedObject(cap));
        uint32_t index = cap_io_page_table_cap_get_capIOPTMappedIndex(cap);
        if (ct) {
            unmapVTDContextEntryAt(ct, index);
        }
    } else {
        vtd_pte_t *parent = VTD_PTE_PTR(cap_io_page_table_cap_get_capIOPTMappedObject(cap));
        if (parent) {
            uint32_t index = cap_io_page_table_cap_get_capIOPTMappedIndex(cap);
            unmapVTDPT(parent, pt, index);
        }
    }
}

exception_t
decodeIA32IOPTInvocation(
    word_t       label,
    uint32_t     length,
    cte_t*       slot,
    cap_t        cap,
    extra_caps_t extraCaps,
    word_t*      buffer
)
{
    cap_t      io_space;
    paddr_t    paddr;
    uint32_t   io_address;
    uint16_t   domain_id;
    vtd_cte_t* vtd_context_slot;
    vtd_pte_t* vtd_pte;
    lookupVTDContextSlot_ret_t lookup_ret;

    if (label == IA32IOPageTableUnmap) {
        unmapIOPTCap(cap);

        cap = cap_io_page_table_cap_set_capIOPTMappedObject(cap, 0);
        cdtUpdate(slot, cap);

        setThreadState(ksCurThread, ThreadState_Restart);
        return EXCEPTION_NONE;
    }

    if (extraCaps.excaprefs[0] == NULL || length < 1) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (label != IA32IOPageTableMap ) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space     = extraCaps.excaprefs[0]->cap;
    io_address   = getSyscallArg(0, buffer);

    if (cap_io_page_table_cap_get_capIOPTMappedObject(cap)) {
        userError("IA32IOPageTableMap: Page table already mapped");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if ((cap_get_capType(io_space) != cap_io_space_cap)) {
        userError("IA32IOPageTableMap: Not a valid IO Space");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    domain_id = cap_io_space_cap_get_capDomainID(io_space);
    if (domain_id == 0) {
        userError("IA32IOPageTableMap: IOSpace has no domain ID");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr(VTD_PTE_PTR(cap_io_page_table_cap_get_capIOPTBasePtr(cap)));
    lookup_ret = lookupVTDContextSlot_helper(io_space);
    vtd_context_slot = lookup_ret.cte + lookup_ret.index;

    if (!vtd_cte_ptr_get_present(vtd_context_slot)) {
        /* 1st Level Page Table */
        vtd_cte_ptr_new(
            vtd_context_slot,
            domain_id,               /* Domain ID */
            mdb_node_get_cdtDepth(slot->cteMDBNode), /* CTE Depth */
            false,                   /* RMRR Mapping */
            ia32KSnumIOPTLevels - 2, /* Address Width (x = levels - 2)       */
            paddr,                   /* Address Space Root                   */
            0,                       /* Translation Type                     */
            true                     /* Present                              */
        );

        flushCacheRange(vtd_context_slot, VTD_CTE_SIZE_BITS);

        cap = cap_io_page_table_cap_set_capIOPTMappedObject(cap, VTD_CTE_REF(lookup_ret.cte));
        cap = cap_io_page_table_cap_set_capIOPTMappedIndex(cap, lookup_ret.index);
        cap = cap_io_page_table_cap_set_capIOPTLevel(cap, 0);
    } else {
        lookupIOPTSlot_ret_t lu_ret;

        vtd_pte = (vtd_pte_t*)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
        lu_ret  = lookupIOPTSlot(vtd_pte, io_address);

        if (lu_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if (lu_ret.level == ia32KSnumIOPTLevels) {
            userError("IA32IOPageTableMap: Cannot map any more levels of page tables");
            current_syscall_error.type =  seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if (vtd_pte_ptr_get_read(lu_ret.iopt + lu_ret.index)) {
            userError("IA32IOPageTableMap: Mapping already exists");
            current_syscall_error.type =  seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        vtd_pte_ptr_new(
            lu_ret.iopt + lu_ret.index,
            paddr,  /* Physical Address         */
            mdb_node_get_cdtDepth(slot->cteMDBNode), /* CTE depth */
            1,      /* Read permission flag     */
            1       /* Write permission flag    */
        );

        flushCacheRange(lu_ret.iopt + lu_ret.index, VTD_PTE_SIZE_BITS);

        cap = cap_io_page_table_cap_set_capIOPTMappedObject(cap, VTD_PTE_REF(lu_ret.iopt));
        cap = cap_io_page_table_cap_set_capIOPTMappedIndex(cap, lu_ret.index);
        cap = cap_io_page_table_cap_set_capIOPTLevel(cap, lu_ret.level);
    }

    cdtUpdate(slot, cap);

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t
decodeIA32IOMapInvocation(
    word_t       label,
    uint32_t     length,
    cte_t*       slot,
    cap_t        cap,
    extra_caps_t extraCaps,
    word_t*      buffer
)
{
    cap_t      io_space;
    uint32_t   io_address;
    uint32_t   domain_id;
    vtd_cte_t* vtd_context_slot;
    vtd_pte_t* vtd_pte;
    paddr_t    paddr;
    vm_rights_t capVMRights;
    vm_rights_t vmRights;
    word_t     w_rightsMask;
    lookupIOPTSlot_ret_t lu_ret;

    if (extraCaps.excaprefs[0] == NULL || length < 2) {
        userError("IA32IOFrameMap: Truncated message");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFSize(cap) != IA32_SmallPage) {
        userError("IA32IOFrameMap: Only 4K frames supported");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFMappedObject(cap) != 0) {
        userError("IA32IOFrameMap: Frame already mapped");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space    = extraCaps.excaprefs[0]->cap;
    w_rightsMask = getSyscallArg(0, buffer);
    io_address  = getSyscallArg(1, buffer);
    paddr       = pptr_to_paddr((void*)cap_frame_cap_get_capFBasePtr(cap));
    capVMRights = cap_frame_cap_get_capFVMRights(cap);
    vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        userError("IA32IOFrameMap: IOSpace cap invalid.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    domain_id = cap_io_space_cap_get_capDomainID(io_space);

    if (domain_id == 0) {
        userError("IA32IOFrameMap: IOSpace has no domain ID");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    vtd_context_slot = lookupVTDContextSlot(io_space);

    if (!vtd_cte_ptr_get_present(vtd_context_slot)) {
        /* 1st Level Page Table is not installed */
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    vtd_pte = (vtd_pte_t*)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
    lu_ret  = lookupIOPTSlot(vtd_pte, io_address);

    if (lu_ret.status != EXCEPTION_NONE || lu_ret.level != ia32KSnumIOPTLevels) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (vtd_pte_ptr_get_read(lu_ret.iopt + lu_ret.index)) {
        userError("IA32IOFrameMap: Mapping already present");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    vtd_pte_ptr_new(
        lu_ret.iopt + lu_ret.index,
        paddr,                                /* Physical Address */
        mdb_node_get_cdtDepth(slot->cteMDBNode),
        1,                                    /* Read permission  */
        WritableFromVMRights(vmRights)        /* Write permission */
    );
    cap = cap_frame_cap_set_capFMappedObject(cap, VTD_PT_REF(lu_ret.iopt));
    cap = cap_frame_cap_set_capFMappedIndex(cap, lu_ret.index);
    cap = cap_frame_cap_set_capFMappedType(cap, IA32_MAPPING_IO);
    cdtUpdate(slot, cap);

    flushCacheRange(lu_ret.iopt + lu_ret.index, VTD_PTE_SIZE_BITS);

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

void unmapIOPage(cap_t cap)
{
    vtd_pte_t *vtd_pt = VTD_PTE_PTR(cap_frame_cap_get_capFMappedObject(cap));
    uint32_t index = cap_frame_cap_get_capFMappedIndex(cap);
    vtd_pte_t *vtd_pte = vtd_pt + index;

    if (!vtd_pt) {
        return;
    }

    vtd_pte_ptr_new(vtd_pte, 0, 0, 0, 0);

    flushCacheRange(vtd_pte, VTD_PTE_SIZE_BITS);
    invalidate_iotlb();
}

#endif
