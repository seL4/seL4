/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

#include <api/syscall.h>
#include <machine/io.h>
#include <kernel/thread.h>
#include <arch/api/invocation.h>
#include <arch/object/iospace.h>
#include <arch/model/statedata.h>
#include <object/structures.h>
#include <arch/linker.h>
#include <plat/machine/smmu.h>


typedef struct lookupIOPDSlot_ret {
    exception_t status;
    iopde_t     *iopdSlot;
} lookupIOPDSlot_ret_t;

typedef struct lookupIOPTSlot_ret {
    exception_t status;
    iopte_t     *ioptSlot;
} lookupIOPTSlot_ret_t;


#define IOPDE_VALID_MASK    0xe0000000
#define IOPTE_EMPTY_MASK    0xe0000000

static bool_t
isIOPDEValid(iopde_t *iopde)
{
    assert(iopde != 0);
    if ((iopde->words[0] & IOPDE_VALID_MASK) == 0) {
        return false;
    }
    return true;
}

static bool_t
isIOPTEEmpty(iopte_t *iopte)
{
    assert(iopte != 0);
    if ((iopte->words[0] & IOPTE_EMPTY_MASK) == 0) {
        return true;
    }
    return false;
}


static lookupIOPDSlot_ret_t
lookupIOPDSlot(iopde_t *iopd, word_t io_address)
{
    lookupIOPDSlot_ret_t ret;
    uint32_t index = plat_smmu_iopd_index(io_address);
    ret.status = EXCEPTION_NONE;
    ret.iopdSlot = iopd + index;
    return ret;
}

static lookupIOPTSlot_ret_t
lookupIOPTSlot(iopde_t *iopd, word_t io_address)
{
    lookupIOPTSlot_ret_t pt_ret;
    uint32_t index;
    iopte_t *pt;

    lookupIOPDSlot_ret_t pd_ret = lookupIOPDSlot(iopd, io_address);
    if (pd_ret.status != EXCEPTION_NONE) {
        pt_ret.status = EXCEPTION_LOOKUP_FAULT;
        pt_ret.ioptSlot = 0;
        return pt_ret;
    }

    if (!isIOPDEValid(pd_ret.iopdSlot) ||
            iopde_ptr_get_page_size(pd_ret.iopdSlot) != iopde_iopde_pt) {
        pt_ret.status = EXCEPTION_LOOKUP_FAULT;
        pt_ret.ioptSlot = 0;
        return pt_ret;
    }

    index = plat_smmu_iopt_index(io_address);
    pt = (iopte_t *)paddr_to_pptr(iopde_iopde_pt_ptr_get_address(pd_ret.iopdSlot));

    if (pt == 0) {
        pt_ret.status = EXCEPTION_LOOKUP_FAULT;
        pt_ret.ioptSlot = 0;
        return pt_ret;
    }

    pt_ret.status = EXCEPTION_NONE;
    pt_ret.ioptSlot = pt + index;
    return pt_ret;
}

BOOT_CODE seL4_SlotRegion
create_iospace_caps(cap_t root_cnode_cap)
{
    seL4_SlotPos start = ndks_boot.slot_pos_cur;
    seL4_SlotPos end = 0;
    cap_t        io_space_cap;
    int i = 0;
    int num_smmu = plat_smmu_init();

    if (num_smmu == 0) {
        printf("SMMU init failuer\n");
        return (seL4_SlotRegion) S_REG_EMPTY;
    }

    /* the 0 is reserved as an invalidASID,
     * assuming each module is assigned an unique ASID
     * and the ASIDs are contiguous
     * */
    for (i = 1; i <= num_smmu; i++) {
        io_space_cap = cap_io_space_cap_new(i, i);
        if (!provide_cap(root_cnode_cap, io_space_cap)) {
            return (seL4_SlotRegion) S_REG_EMPTY;
        }
    }
    end = ndks_boot.slot_pos_cur;
    printf("Region [%x to %x) for SMMU caps\n", (unsigned int)start, (unsigned int)end);
    return (seL4_SlotRegion) {
        start, end
    };
}


exception_t
decodeARMIOPTInvocation(
    word_t       invLabel,
    uint32_t     length,
    cte_t*       slot,
    cap_t        cap,
    extra_caps_t excaps,
    word_t*      buffer
)
{
    cap_t      io_space;
    word_t     io_address;
    word_t     paddr;
    uint16_t   module_id;
    uint32_t   asid;
    iopde_t    *pd;
    lookupIOPDSlot_ret_t    lu_ret;

    if (invLabel == ARMIOPageTableUnmap) {
        deleteIOPageTable(slot->cap);
        slot->cap = cap_io_page_table_cap_set_capIOPTIsMapped(slot->cap, 0);

        setThreadState(ksCurThread, ThreadState_Restart);
        return EXCEPTION_NONE;
    }

    if (excaps.excaprefs[0] == NULL || length < 1) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (invLabel != ARMIOPageTableMap ) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space     = excaps.excaprefs[0]->cap;
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

    module_id = cap_io_space_cap_get_capModuleID(io_space);
    asid = plat_smmu_get_asid_by_module_id(module_id);
    if (asid == asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void *)cap_io_page_table_cap_get_capIOPTBasePtr(cap));
    pd = (iopde_t *)plat_smmu_lookup_iopd_by_asid(asid);

    if (pd == 0) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    lu_ret = lookupIOPDSlot(pd, io_address);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (isIOPDEValid(lu_ret.iopdSlot)) {
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    iopde_iopde_pt_ptr_new(
        lu_ret.iopdSlot,
        1,      /* read */
        1,      /* write */
        1,      /* nonsecure */
        paddr   /* address */
    );

    cleanCacheRange_RAM((word_t)lu_ret.iopdSlot,
                        ((word_t)lu_ret.iopdSlot) + sizeof(iopde_t),
                        addrFromPPtr(lu_ret.iopdSlot));

    plat_smmu_tlb_flush_all();
    plat_smmu_ptc_flush_all();

    cap = cap_io_page_table_cap_set_capIOPTIsMapped(cap, 1);
    cap = cap_io_page_table_cap_set_capIOPTASID(cap, asid);
    cap = cap_io_page_table_cap_set_capIOPTMappedAddress(cap, io_address);

    slot->cap = cap;

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t
decodeARMIOMapInvocation(
    word_t       invLabel,
    uint32_t     length,
    cte_t*       slot,
    cap_t        cap,
    extra_caps_t excaps,
    word_t*      buffer
)
{
    cap_t      io_space;
    paddr_t    io_address;
    paddr_t    paddr;
    uint32_t   module_id;
    uint32_t   asid;
    iopde_t    *pd;
    vm_rights_t     frame_cap_rights;
    cap_rights_t    dma_cap_rights_mask;
    lookupIOPTSlot_ret_t lu_ret;

    if (excaps.excaprefs[0] == NULL || length < 2) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (generic_frame_cap_get_capFSize(cap) != ARMSmallPage) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_small_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space    = excaps.excaprefs[0]->cap;
    io_address  = getSyscallArg(1, buffer) & ~MASK(PAGE_BITS);
    paddr       = pptr_to_paddr((void*)cap_small_frame_cap_get_capFBasePtr(cap));

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    module_id = cap_io_space_cap_get_capModuleID(io_space);
    asid = plat_smmu_get_asid_by_module_id(module_id);

    if (asid == asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    pd = (iopde_t *)plat_smmu_lookup_iopd_by_asid(asid);
    if (pd == 0) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    lu_ret = lookupIOPTSlot(pd, io_address);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (!isIOPTEEmpty(lu_ret.ioptSlot)) {
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }
    frame_cap_rights = cap_small_frame_cap_get_capFVMRights(cap);
    dma_cap_rights_mask = rightsFromWord(getSyscallArg(0, buffer));

    if ((frame_cap_rights == VMReadOnly) && cap_rights_get_capAllowRead(dma_cap_rights_mask)) {
        /* read only */
        iopte_ptr_new(
            lu_ret.ioptSlot,
            1,
            0,
            1,
            paddr
        );
    } else if (frame_cap_rights == VMReadWrite) {
        if (cap_rights_get_capAllowRead(dma_cap_rights_mask) &&
                !cap_rights_get_capAllowWrite(dma_cap_rights_mask)) {
            /* read only */
            iopte_ptr_new(
                lu_ret.ioptSlot,
                1,      /* read         */
                0,      /* write        */
                1,      /* nonsecure    */
                paddr
            );
        } else if (!cap_rights_get_capAllowRead(dma_cap_rights_mask) &&
                   cap_rights_get_capAllowWrite(dma_cap_rights_mask)) {
            /* write only */
            iopte_ptr_new(
                lu_ret.ioptSlot,
                0,
                1,
                1,
                paddr
            );
        } else if (cap_rights_get_capAllowRead(dma_cap_rights_mask) &&
                   cap_rights_get_capAllowWrite(dma_cap_rights_mask)) {
            /* read write */
            iopte_ptr_new(
                lu_ret.ioptSlot,
                1,
                1,
                1,
                paddr
            );
        } else {
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

    } else {
        /* VMKernelOnly */
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cleanCacheRange_RAM((word_t)lu_ret.ioptSlot,
                        ((word_t)lu_ret.ioptSlot) + sizeof(iopte_t),
                        addrFromPPtr(lu_ret.ioptSlot));

    plat_smmu_tlb_flush_all();
    plat_smmu_ptc_flush_all();

#ifdef CONFIG_ARM_SMMU
    cap = cap_small_frame_cap_set_capFIsIOSpace(cap, 1);
#endif
    cap = cap_small_frame_cap_set_capFMappedASID(cap, asid);
    cap = cap_small_frame_cap_set_capFMappedAddress(cap, io_address);
    slot->cap = cap;

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}


void
deleteIOPageTable(cap_t io_pt_cap)
{

    uint32_t asid;
    iopde_t *pd;
    lookupIOPDSlot_ret_t lu_ret;
    word_t io_address;
    if (cap_io_page_table_cap_get_capIOPTIsMapped(io_pt_cap)) {
        io_pt_cap = cap_io_page_table_cap_set_capIOPTIsMapped(io_pt_cap, 0);
        asid = cap_io_page_table_cap_get_capIOPTASID(io_pt_cap);
        pd = (iopde_t *)plat_smmu_lookup_iopd_by_asid(asid);
        io_address = cap_io_page_table_cap_get_capIOPTMappedAddress(io_pt_cap);

        if (pd == 0) {
            return;
        }

        lu_ret = lookupIOPDSlot(pd, io_address);
        if (lu_ret.status != EXCEPTION_NONE) {
            return;
        }

        if (isIOPDEValid(lu_ret.iopdSlot) &&
                iopde_ptr_get_page_size(lu_ret.iopdSlot) == iopde_iopde_pt &&
                iopde_iopde_pt_ptr_get_address(lu_ret.iopdSlot) != (pptr_to_paddr((void *)cap_io_page_table_cap_get_capIOPTBasePtr(io_pt_cap)))) {
            return;
        }

        iopde_iopde_pt_ptr_new(lu_ret.iopdSlot, 0, 0, 0, 0);
        cleanCacheRange_RAM((word_t)lu_ret.iopdSlot,
                            ((word_t)lu_ret.iopdSlot) + sizeof(iopde_t),
                            addrFromPPtr(lu_ret.iopdSlot));


        /* nice to have: flush by address and asid */
        plat_smmu_tlb_flush_all();
        plat_smmu_ptc_flush_all();
    }
}

void
unmapIOPage(cap_t cap)
{
    lookupIOPTSlot_ret_t lu_ret;
    iopde_t *pd;
    word_t  io_address;
    uint32_t asid;

    io_address = cap_small_frame_cap_get_capFMappedAddress(cap);
    asid = cap_small_frame_cap_get_capFMappedASID(cap);
    pd = (iopde_t *)plat_smmu_lookup_iopd_by_asid(asid);

    if (pd == 0) {
        return;
    }

    lu_ret = lookupIOPTSlot(pd, io_address);

    if (lu_ret.status != EXCEPTION_NONE) {
        return;
    }
    if (iopte_ptr_get_address(lu_ret.ioptSlot) != pptr_to_paddr((void *)cap_small_frame_cap_get_capFBasePtr(cap))) {
        return;
    }

    iopte_ptr_new(lu_ret.ioptSlot, 0, 0, 0, 0);
    cleanCacheRange_RAM((word_t)lu_ret.ioptSlot,
                        ((word_t)lu_ret.ioptSlot) + sizeof(iopte_t),
                        addrFromPPtr(lu_ret.ioptSlot));

    plat_smmu_tlb_flush_all();
    plat_smmu_ptc_flush_all();
    return;
}

void clearIOPageDirectory(cap_t cap)
{
    iopde_t  *pd;
    uint32_t asid = cap_io_space_cap_get_capModuleID(cap);
    word_t   size = BIT((ARM_IOPDE_SIZE_BITS + ARM_IOPD_BITS));
    pd = (iopde_t *)plat_smmu_lookup_iopd_by_asid(asid);

    if (pd == 0) {
        return;
    }
    memset((void *)pd, 0, size);
    cleanCacheRange_RAM((word_t)pd, (word_t)pd + size, addrFromPPtr(pd));

    plat_smmu_tlb_flush_all();
    plat_smmu_ptc_flush_all();
    return;
}

exception_t
decodeARMIOUnMapInvocation(
    word_t       invLabel,
    uint32_t     length,
    cte_t*       slot,
    cap_t        cap,
    extra_caps_t excaps
)
{
    unmapIOPage(slot->cap);
    slot->cap = cap_small_frame_cap_set_capFMappedAddress(slot->cap, 0);
#ifdef CONFIG_ARM_SMMU
    slot->cap = cap_small_frame_cap_set_capFIsIOSpace(slot->cap, 0);
#endif
    slot->cap = cap_small_frame_cap_set_capFMappedASID(slot->cap, asidInvalid);

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t
decodeARMIOSpaceInvocation(word_t invLabel, cap_t cap)
{
    userError("IOSpace capability has no invocations");
    current_syscall_error.type = seL4_IllegalOperation;
    return EXCEPTION_SYSCALL_ERROR;
}

