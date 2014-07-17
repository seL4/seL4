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
#include <plat/machine/pci.h>

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

static vtd_cte_t* lookup_vtd_context_slot(cap_t cap)
{
    uint32_t   vtd_root_index;
    uint32_t   vtd_context_index;
    uint32_t   pci_request_id;
    vtd_rte_t* vtd_root_slot;
    vtd_cte_t* vtd_context;
    vtd_cte_t* vtd_context_slot;

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
    vtd_root_slot = ia32KSvtdRootTable + vtd_root_index;

    vtd_context = (vtd_cte_t*)paddr_to_pptr(vtd_rte_ptr_get_ctp(vtd_root_slot));
    vtd_context_index = (get_pci_dev(pci_request_id) << 3) | get_pci_fun(pci_request_id);
    vtd_context_slot = &vtd_context[vtd_context_index];

    return vtd_context_slot;
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
        ret.ioptSlot = vtd_pte_slot;
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
        ret.ioptSlot    = 0;
        ret.level       = 0;
        ret.status      = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        return lookupIOPTSlot_helper(iopt, io_address >> PAGE_BITS, ia32KSnumIOPTLevels - 1);
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
    uint32_t   pci_request_id;
    uint32_t   io_address;
    uint16_t   domain_id;
    vtd_cte_t* vtd_context_slot;
    vtd_pte_t* vtd_pte;

    if (extraCaps.excaprefs[0] == NULL || length < 1) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (label != IA32IOPageTableMap ) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space     = extraCaps.excaprefs[0]->cap;
    io_address   = getSyscallArg(0, buffer) & ~MASK(PAGE_BITS);

    if (cap_io_page_table_cap_get_capIOPTIsMapped(cap)) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    pci_request_id = cap_io_space_cap_get_capPCIDevice(cap);
    domain_id = cap_io_space_cap_get_capDomainID(cap);
    if (pci_request_id == asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr(VTD_PTE_PTR(cap_io_page_table_cap_get_capIOPTBasePtr(cap)));
    vtd_context_slot = lookup_vtd_context_slot(io_space);

    if (!vtd_cte_ptr_get_present(vtd_context_slot)) {
        /* 1st Level Page Table */
        vtd_cte_ptr_new(
            vtd_context_slot,
            domain_id,               /* Domain ID */
            ia32KSnumIOPTLevels - 2, /* Address Width (x = levels - 2)       */
            paddr,                   /* Address Space Root                   */
            0,                       /* Translation Type                     */
            true                     /* Present                              */
        );

        flushCacheRange(vtd_context_slot, VTD_CTE_SIZE_BITS);

        cap = cap_io_page_table_cap_set_capIOPTIsMapped(cap, 1);
        cap = cap_io_page_table_cap_set_capIOPTLevel(cap, 0);
        cap = cap_io_page_table_cap_set_capIOPTIOASID(cap, pci_request_id);
    } else {
        lookupIOPTSlot_ret_t lu_ret;

        vtd_pte = (vtd_pte_t*)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
        lu_ret  = lookupIOPTSlot(vtd_pte, io_address);

        if (lu_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        vtd_pte_ptr_new(
            lu_ret.ioptSlot,
            paddr,  /* Physical Address         */
            1,      /* Read permission flag     */
            1       /* Write permission flag    */
        );

        flushCacheRange(lu_ret.ioptSlot, VTD_PTE_SIZE_BITS);

        cap = cap_io_page_table_cap_set_capIOPTIsMapped(cap, 1);
        cap = cap_io_page_table_cap_set_capIOPTLevel(cap, lu_ret.level);
        cap = cap_io_page_table_cap_set_capIOPTIOASID(cap, pci_request_id);
        cap = cap_io_page_table_cap_set_capIOPTMappedAddress(cap, io_address);
    }

    slot->cap = cap;

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
    uint32_t   pci_request_id;
    vtd_cte_t* vtd_context_slot;
    vtd_pte_t* vtd_pte;
    paddr_t    paddr;

    if (extraCaps.excaprefs[0] == NULL || length < 2) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFSize(cap) != IA32_4K) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space    = extraCaps.excaprefs[0]->cap;
    io_address  = getSyscallArg(1, buffer) & ~MASK(PAGE_BITS);
    paddr       = pptr_to_paddr((void*)cap_frame_cap_get_capFBasePtr(cap));

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    pci_request_id = cap_io_space_cap_get_capPCIDevice(io_space);

    if (pci_request_id == asidInvalid) {
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
    } else {
        lookupIOPTSlot_ret_t lu_ret;
        vm_rights_t          frame_cap_rights;
        cap_rights_t         dma_cap_rights_mask;

        vtd_pte = (vtd_pte_t*)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
        lu_ret  = lookupIOPTSlot(vtd_pte, io_address);

        if (lu_ret.status != EXCEPTION_NONE || lu_ret.level != ia32KSnumIOPTLevels) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            return EXCEPTION_SYSCALL_ERROR;
        }

        dma_cap_rights_mask = rightsFromWord(getSyscallArg(0, buffer));
        frame_cap_rights    = cap_frame_cap_get_capFVMRights(cap);

        if ((frame_cap_rights == VMReadOnly) && cap_rights_get_capAllowRead(dma_cap_rights_mask)) {
            /* Read Only */
            vtd_pte_ptr_new(
                lu_ret.ioptSlot,
                paddr,  /* Physical Address */
                0,      /* Read permission  */
                1       /* Write permission */
            );
        } else if (frame_cap_rights == VMReadWrite) {
            if (cap_rights_get_capAllowRead(dma_cap_rights_mask) && !cap_rights_get_capAllowWrite(dma_cap_rights_mask)) {
                /* Read Only */
                vtd_pte_ptr_new(
                    lu_ret.ioptSlot,
                    paddr,  /* Physical Address */
                    0,      /* Read permission  */
                    1       /* Write permission */
                );
            } else if (!cap_rights_get_capAllowRead(dma_cap_rights_mask) && cap_rights_get_capAllowWrite(dma_cap_rights_mask)) {
                /* Write Only */
                vtd_pte_ptr_new(
                    lu_ret.ioptSlot,
                    paddr,  /* Physical Address */
                    1,      /* Read permission  */
                    0       /* Write permission */
                );

            } else if (cap_rights_get_capAllowRead(dma_cap_rights_mask) && cap_rights_get_capAllowWrite(dma_cap_rights_mask)) {
                /* Read Write */
                vtd_pte_ptr_new(
                    lu_ret.ioptSlot,
                    paddr,  /* Physical Address */
                    1,      /* Read permission  */
                    1       /* Write permission */
                );
            } else {
                current_syscall_error.type = seL4_InvalidArgument;
                current_syscall_error.invalidArgumentNumber = 0;
                return EXCEPTION_SYSCALL_ERROR;
            }
        } else {
            /* We are dealing with VMKernelOnly */
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }
        cap = cap_frame_cap_set_capFIsIOSpace(cap, 1);
        cap = cap_frame_cap_set_capFMappedASID(cap, pci_request_id);
        cap = cap_frame_cap_set_capFMappedAddress(cap, io_address);

        flushCacheRange(lu_ret.ioptSlot, VTD_PTE_SIZE_BITS);
    }
    slot->cap = cap;

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

void deleteIOPageTable(cap_t io_pt_cap)
{
    lookupIOPTSlot_ret_t lu_ret;
    uint32_t             level;
    uint32_t             io_address;
    vtd_cte_t*           vtd_context_slot;
    vtd_pte_t*           vtd_pte;

    if (cap_io_page_table_cap_get_capIOPTIsMapped(io_pt_cap)) {
        io_pt_cap = cap_io_page_table_cap_set_capIOPTIsMapped(io_pt_cap, 0);
        level = cap_io_page_table_cap_get_capIOPTLevel(io_pt_cap);
        vtd_context_slot = lookup_vtd_context_slot(io_pt_cap);
        vtd_pte = (vtd_pte_t*)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));
        if (level == 0) {
            /* if we have been overmapped or something */
            if (pptr_to_paddr(vtd_pte) != cap_io_page_table_cap_get_capIOPTBasePtr(io_pt_cap)) {
                return;
            }
            vtd_cte_ptr_new(
                vtd_context_slot,
                0,  /* Domain ID          */
                0,  /* Address Width      */
                0,  /* Address Space Root */
                0,  /* Translation Type   */
                0   /* Present            */
            );
            flushCacheRange(vtd_context_slot, VTD_CTE_SIZE_BITS);
        } else {
            io_address = cap_io_page_table_cap_get_capIOPTMappedAddress(io_pt_cap);
            lu_ret = lookupIOPTSlot_helper(vtd_pte, io_address >> PAGE_BITS, level - 1);
            /* if we have been overmapped or something */
            if (lu_ret.status != EXCEPTION_NONE || lu_ret.level + 1 != level) {
                return;
            }
            if (vtd_pte_ptr_get_addr(lu_ret.ioptSlot) != cap_io_page_table_cap_get_capIOPTBasePtr(io_pt_cap)) {
                return;
            }
            vtd_pte_ptr_new(
                lu_ret.ioptSlot,
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
    uint32_t             io_address;
    vtd_cte_t*           vtd_context_slot;
    vtd_pte_t*           vtd_pte;

    io_address  = cap_frame_cap_get_capFMappedAddress(cap);
    vtd_context_slot = lookup_vtd_context_slot(cap);
    vtd_pte = (vtd_pte_t*)paddr_to_pptr(vtd_cte_ptr_get_asr(vtd_context_slot));

    lu_ret  = lookupIOPTSlot(vtd_pte, io_address);
    if (lu_ret.status != EXCEPTION_NONE || lu_ret.level != ia32KSnumIOPTLevels) {
        return;
    }

    vtd_pte_ptr_new(
        lu_ret.ioptSlot,
        0,  /* Physical Address */
        0,  /* Read Permission  */
        0   /* Write Permission */
    );

    flushCacheRange(lu_ret.ioptSlot, VTD_PTE_SIZE_BITS);
    invalidate_iotlb();
}

exception_t
decodeIA32IOUnMapInvocation(
    word_t       label,
    uint32_t     length,
    cte_t*       slot,
    cap_t        cap,
    extra_caps_t extraCaps
)
{
    unmapIOPage(slot->cap);
    slot->cap = cap_frame_cap_set_capFMappedAddress(slot->cap, 0);
    slot->cap = cap_frame_cap_set_capFIsIOSpace(slot->cap, 0);
    slot->cap = cap_frame_cap_set_capFMappedASID(slot->cap, asidInvalid);

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t decodeIA32IOSpaceInvocation(word_t label, cap_t cap)
{
    vtd_cte_t *cte;
    if (label != IA32IOSpaceRemovePassthrough) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
    cte = lookup_vtd_context_slot(cap);
    *cte = vtd_cte_set_translation_type(*cte, 0);
    flushCacheRange(cte, VTD_CTE_SIZE_BITS);
    invalidate_iotlb();
    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

#endif
