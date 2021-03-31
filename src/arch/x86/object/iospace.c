/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_IOMMU

#include <api/syscall.h>
#include <machine/io.h>
#include <kernel/thread.h>
#include <arch/api/invocation.h>
#include <arch/object/iospace.h>
#include <arch/model/statedata.h>
#include <linker.h>
#include <plat/machine/intel-vtd.h>


typedef struct lookupVTDContextSlot_ret {
    vtd_cte_t   *cte;
    word_t      index;
} lookupVTDContextSlot_ret_t;


BOOT_CODE cap_t master_iospace_cap(void)
{
    if (x86KSnumDrhu == 0) {
        return cap_null_cap_new();
    }

    return
        cap_io_space_cap_new(
            0,              /* capDomainID  */
            0              /* capPCIDevice */
        );
}

static vtd_cte_t *lookup_vtd_context_slot(cap_t cap)
{
    uint32_t   vtd_root_index;
    uint32_t   vtd_context_index;
    uint32_t   pci_request_id;
    vtd_rte_t *vtd_root_slot;
    vtd_cte_t *vtd_context;
    vtd_cte_t *vtd_context_slot;

    switch (cap_get_capType(cap)) {
    case cap_io_space_cap:
        pci_request_id = cap_io_space_cap_get_capPCIDevice(cap);
        break;

    case cap_io_page_table_cap:
        pci_request_id = cap_io_page_table_cap_get_capIOPTIOASID(cap);
        break;

    case cap_frame_cap:
        pci_request_id = cap_frame_cap_get_capFMappedASID(cap);
        break;

    default:
        fail("Invalid cap type");
    }

    vtd_root_index = get_pci_bus(pci_request_id);
    vtd_root_slot = x86KSvtdRootTable + vtd_root_index;

    vtd_context = (vtd_cte_t *)paddr_to_pptr(vtd_rte_ptr_get_ctp(vtd_root_slot));
    vtd_context_index = (get_pci_dev(pci_request_id) << 3) | get_pci_fun(pci_request_id);
    vtd_context_slot = &vtd_context[vtd_context_index];

    return vtd_context_slot;
}

static lookupIOPTSlot_ret_t lookupIOPTSlot_resolve_levels(vtd_pte_t *iopt, word_t translation,
                                                          word_t levels_to_resolve, word_t levels_remaining)
{
    lookupIOPTSlot_ret_t ret;

    word_t      iopt_index = 0;
    vtd_pte_t   *iopt_slot = 0;
    vtd_pte_t   *next_iopt_slot = 0;

    if (iopt == 0) {
        ret.ioptSlot = 0;
        ret.level = levels_remaining;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    iopt_index = (translation  >> (VTD_PT_INDEX_BITS * (x86KSnumIOPTLevels - 1 - (levels_to_resolve - levels_remaining)))) &
                 MASK(VTD_PT_INDEX_BITS);
    iopt_slot = iopt + iopt_index;

    if (!vtd_pte_ptr_get_write(iopt_slot) || levels_remaining == 0) {
        ret.ioptSlot = iopt_slot;
        ret.level = levels_remaining;
        ret.status = EXCEPTION_NONE;
        return ret;
    }
    next_iopt_slot = (vtd_pte_t *)paddr_to_pptr(vtd_pte_ptr_get_addr(iopt_slot));
    return lookupIOPTSlot_resolve_levels(next_iopt_slot, translation, levels_to_resolve, levels_remaining - 1);
}


static inline lookupIOPTSlot_ret_t lookupIOPTSlot(vtd_pte_t *iopt, word_t io_address)
{
    lookupIOPTSlot_ret_t ret;

    if (iopt == 0) {
        ret.ioptSlot    = 0;
        ret.level       = 0;
        ret.status      = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        return lookupIOPTSlot_resolve_levels(iopt, io_address >> PAGE_BITS,
                                             x86KSnumIOPTLevels - 1, x86KSnumIOPTLevels - 1);
    }
}

void unmapVTDContextEntry(cap_t cap)
{
    vtd_cte_t *cte = lookup_vtd_context_slot(cap);
    assert(cte != 0);
    *cte = vtd_cte_new(
               0,
               false,
               0,
               0,
               0,
               false
           );

    flushCacheRange(cte, VTD_CTE_SIZE_BITS);
    invalidate_iotlb();
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return;
}

static exception_t performX86IOPTInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    deleteIOPageTable(cap);
    cap = cap_io_page_table_cap_set_capIOPTIsMapped(cap, 0);
    ctSlot->cap = cap;

    return EXCEPTION_NONE;
}

static exception_t performX86IOPTInvocationMapContextRoot(cap_t cap, cte_t *ctSlot, vtd_cte_t vtd_cte,
                                                          vtd_cte_t *vtd_context_slot)
{
    *vtd_context_slot = vtd_cte;
    flushCacheRange(vtd_context_slot, VTD_CTE_SIZE_BITS);
    ctSlot->cap = cap;

    return EXCEPTION_NONE;
}

static exception_t performX86IOPTInvocationMapPT(cap_t cap, cte_t *ctSlot, vtd_pte_t iopte, vtd_pte_t *ioptSlot)
{
    *ioptSlot = iopte;
    flushCacheRange(ioptSlot, VTD_PTE_SIZE_BITS);
    ctSlot->cap = cap;

    return EXCEPTION_NONE;
}

exception_t decodeX86IOPTInvocation(
    word_t       invLabel,
    word_t       length,
    cte_t       *slot,
    cap_t        cap,
    word_t      *buffer
)
{
    cap_t      io_space;
    paddr_t    paddr;
    uint32_t   pci_request_id;
    word_t   io_address;
    uint16_t   domain_id;
    vtd_cte_t *vtd_context_slot;
    vtd_pte_t *vtd_pte;

    if (invLabel == X86IOPageTableUnmap) {

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performX86IOPTInvocationUnmap(cap, slot);
    }

    if (invLabel != X86IOPageTableMap) {
        userError("X86IOPageTable: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (current_extra_caps.excaprefs[0] == NULL || length < 1) {
        userError("X86IOPageTableMap: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space     = current_extra_caps.excaprefs[0]->cap;
    io_address   = getSyscallArg(0, buffer) & ~MASK(VTD_PT_INDEX_BITS + seL4_PageBits);

    if (cap_io_page_table_cap_get_capIOPTIsMapped(cap)) {
        userError("X86IOPageTableMap: IO page table is already mapped.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        userError("X86IOPageTableMap: Invalid IO space capability.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    pci_request_id = cap_io_space_cap_get_capPCIDevice(io_space);
    domain_id = cap_io_space_cap_get_capDomainID(io_space);
    if (pci_request_id == asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr(VTD_PTE_PTR(cap_io_page_table_cap_get_capIOPTBasePtr(cap)));
    vtd_context_slot = lookup_vtd_context_slot(io_space);

    if (!vtd_cte_ptr_get_present(vtd_context_slot)) {

        /* 1st Level Page Table */
        vtd_cte_t vtd_cte = vtd_cte_new(
                                domain_id,                  /* domain ID                   */
                                false,                      /* RMRR                        */
                                x86KSnumIOPTLevels - 2,     /* addr width (x = levels - 2) */
                                paddr,                      /* address space root          */
                                0,                          /* translation type            */
                                true                        /* present                     */
                            );

        cap = cap_io_page_table_cap_set_capIOPTIsMapped(cap, 1);
        cap = cap_io_page_table_cap_set_capIOPTLevel(cap, 0);
        cap = cap_io_page_table_cap_set_capIOPTIOASID(cap, pci_request_id);

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performX86IOPTInvocationMapContextRoot(cap, slot, vtd_cte, vtd_context_slot);
    } else {
        lookupIOPTSlot_ret_t lu_ret;
        vtd_pte_t   iopte;

        vtd_pte = (vtd_pte_t *)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
        lu_ret  = lookupIOPTSlot(vtd_pte, io_address);

        if (lu_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        lu_ret.level = x86KSnumIOPTLevels - lu_ret.level;
        if (vtd_pte_ptr_get_addr(lu_ret.ioptSlot) != 0) {
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        iopte = vtd_pte_new(
                    paddr,      /* physical addr            */
                    1,          /* write permission flag    */
                    1           /* read  permission flag    */
                );

        cap = cap_io_page_table_cap_set_capIOPTIsMapped(cap, 1);
        cap = cap_io_page_table_cap_set_capIOPTLevel(cap, lu_ret.level);
        cap = cap_io_page_table_cap_set_capIOPTIOASID(cap, pci_request_id);
        cap = cap_io_page_table_cap_set_capIOPTMappedAddress(cap, io_address);

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return performX86IOPTInvocationMapPT(cap, slot, iopte, lu_ret.ioptSlot);
    }
}

static exception_t performX86IOInvocationMap(cap_t cap, cte_t *ctSlot, vtd_pte_t iopte, vtd_pte_t *ioptSlot)
{
    ctSlot->cap = cap;
    *ioptSlot = iopte;
    flushCacheRange(ioptSlot, VTD_PTE_SIZE_BITS);

    return EXCEPTION_NONE;
}


exception_t decodeX86IOMapInvocation(
    word_t       length,
    cte_t       *slot,
    cap_t        cap,
    word_t      *buffer
)
{
    cap_t      io_space;
    word_t     io_address;
    uint32_t   pci_request_id;
    vtd_cte_t *vtd_context_slot;
    vtd_pte_t *vtd_pte;
    vtd_pte_t  iopte;
    paddr_t    paddr;
    lookupIOPTSlot_ret_t lu_ret;
    vm_rights_t frame_cap_rights;
    seL4_CapRights_t dma_cap_rights_mask;

    if (current_extra_caps.excaprefs[0] == NULL || length < 2) {
        userError("X86PageMapIO: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFSize(cap) != X86_SmallPage) {
        userError("X86PageMapIO: Invalid page size.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        userError("X86PageMapIO: Page already mapped.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space    = current_extra_caps.excaprefs[0]->cap;
    io_address  = getSyscallArg(1, buffer) & ~MASK(PAGE_BITS);
    paddr       = pptr_to_paddr((void *)cap_frame_cap_get_capFBasePtr(cap));

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        userError("X86PageMapIO: Invalid IO space capability.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    pci_request_id = cap_io_space_cap_get_capPCIDevice(io_space);

    if (pci_request_id == asidInvalid) {
        userError("X86PageMapIO: Invalid PCI device.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    vtd_context_slot = lookup_vtd_context_slot(io_space);

    if (!vtd_cte_ptr_get_present(vtd_context_slot)) {
        /* 1st Level Page Table is not installed */
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    vtd_pte = (vtd_pte_t *)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
    lu_ret  = lookupIOPTSlot(vtd_pte, io_address);
    if (lu_ret.status != EXCEPTION_NONE || lu_ret.level != 0) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (vtd_pte_ptr_get_addr(lu_ret.ioptSlot) != 0) {
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    dma_cap_rights_mask = rightsFromWord(getSyscallArg(0, buffer));
    frame_cap_rights    = cap_frame_cap_get_capFVMRights(cap);

    bool_t write = seL4_CapRights_get_capAllowWrite(dma_cap_rights_mask) && (frame_cap_rights == VMReadWrite);
    bool_t read = seL4_CapRights_get_capAllowRead(dma_cap_rights_mask) && (frame_cap_rights != VMKernelOnly);
    if (write || read) {
        iopte = vtd_pte_new(paddr, !!write, !!read);
    } else {
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap = cap_frame_cap_set_capFMapType(cap, X86_MappingIOSpace);
    cap = cap_frame_cap_set_capFMappedASID(cap, pci_request_id);
    cap = cap_frame_cap_set_capFMappedAddress(cap, io_address);

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return performX86IOInvocationMap(cap, slot, iopte, lu_ret.ioptSlot);
}

void deleteIOPageTable(cap_t io_pt_cap)
{
    lookupIOPTSlot_ret_t lu_ret;
    uint32_t             level;
    word_t               io_address;
    vtd_cte_t           *vtd_context_slot;
    vtd_pte_t           *vtd_pte;

    if (cap_io_page_table_cap_get_capIOPTIsMapped(io_pt_cap)) {
        io_pt_cap = cap_io_page_table_cap_set_capIOPTIsMapped(io_pt_cap, 0);
        level = cap_io_page_table_cap_get_capIOPTLevel(io_pt_cap);
        vtd_context_slot = lookup_vtd_context_slot(io_pt_cap);

        if (!vtd_cte_ptr_get_present(vtd_context_slot)) {
            return;
        }

        vtd_pte = (vtd_pte_t *)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));

        if (level == 0) {
            /* if we have been overmapped or something */
            if (pptr_to_paddr(vtd_pte) != pptr_to_paddr((void *)cap_io_page_table_cap_get_capIOPTBasePtr(io_pt_cap))) {
                return;
            }
            *vtd_context_slot = vtd_cte_new(
                                    0,      /* Domain ID          */
                                    false,  /* RMRR               */
                                    0,      /* Address Width      */
                                    0,      /* Address Space Root */
                                    0,      /* Translation Type   */
                                    0       /* Present            */
                                );
            flushCacheRange(vtd_context_slot, VTD_CTE_SIZE_BITS);
        } else {
            io_address = cap_io_page_table_cap_get_capIOPTMappedAddress(io_pt_cap);
            lu_ret = lookupIOPTSlot_resolve_levels(vtd_pte, io_address >> PAGE_BITS, level - 1, level - 1);

            /* if we have been overmapped or something */
            if (lu_ret.status != EXCEPTION_NONE || lu_ret.level != 0) {
                return;
            }
            if (vtd_pte_ptr_get_addr(lu_ret.ioptSlot) != pptr_to_paddr((void *)cap_io_page_table_cap_get_capIOPTBasePtr(
                                                                           io_pt_cap))) {
                return;
            }
            *lu_ret.ioptSlot = vtd_pte_new(
                                   0,  /* Physical Address */
                                   0,  /* Read Permission  */
                                   0   /* Write Permission */
                               );
            flushCacheRange(lu_ret.ioptSlot, VTD_PTE_SIZE_BITS);
        }
        invalidate_iotlb();
    }
}

void unmapIOPage(cap_t cap)
{
    lookupIOPTSlot_ret_t lu_ret;
    word_t               io_address;
    vtd_cte_t           *vtd_context_slot;
    vtd_pte_t           *vtd_pte;

    io_address  = cap_frame_cap_get_capFMappedAddress(cap);
    vtd_context_slot = lookup_vtd_context_slot(cap);


    if (!vtd_cte_ptr_get_present(vtd_context_slot)) {
        return;
    }

    vtd_pte = (vtd_pte_t *)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));

    lu_ret  = lookupIOPTSlot(vtd_pte, io_address);
    if (lu_ret.status != EXCEPTION_NONE || lu_ret.level != 0) {
        return;
    }

    if (vtd_pte_ptr_get_addr(lu_ret.ioptSlot) != pptr_to_paddr((void *)cap_frame_cap_get_capFBasePtr(cap))) {
        return;
    }

    *lu_ret.ioptSlot = vtd_pte_new(
                           0,  /* Physical Address */
                           0,  /* Read Permission  */
                           0   /* Write Permission */
                       );

    flushCacheRange(lu_ret.ioptSlot, VTD_PTE_SIZE_BITS);
    invalidate_iotlb();
}

exception_t performX86IOUnMapInvocation(cap_t cap, cte_t *ctSlot)
{
    unmapIOPage(ctSlot->cap);

    ctSlot->cap = cap_frame_cap_set_capFMappedAddress(ctSlot->cap, 0);
    ctSlot->cap = cap_frame_cap_set_capFMapType(ctSlot->cap, X86_MappingNone);
    ctSlot->cap = cap_frame_cap_set_capFMappedASID(ctSlot->cap, asidInvalid);

    return EXCEPTION_NONE;
}

exception_t decodeX86IOSpaceInvocation(word_t invLabel, cap_t cap)
{
    userError("IOSpace capability has no invocations");
    current_syscall_error.type = seL4_IllegalOperation;
    return EXCEPTION_SYSCALL_ERROR;
}

#endif /* CONFIG_IOMMU */
