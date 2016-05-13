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
    index = plat_smmu_iopt_index(io_address);
    pt = (iopte_t *)paddr_to_pptr(iopde_iopde_pt_ptr_get_address(pd_ret.iopdSlot));
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

    for (i = 0; i < num_smmu; i++) {
        io_space_cap = cap_io_space_cap_new(i, i);  
        if (!provide_cap(root_cnode_cap, io_space_cap)) {
            return (seL4_SlotRegion) S_REG_EMPTY;
        }
    }
    return (seL4_SlotRegion) {start, end};
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
    uint16_t   module_id;
    (void)io_address;

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

    module_id= cap_io_space_cap_get_capModuleID(io_space);
    if (module_id == asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }


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
    (void)paddr;
    (void)io_address;

    if (excaps.excaprefs[0] == NULL || length < 2) {
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFSize(cap) != ARMSmallPage) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space    = excaps.excaprefs[0]->cap;
    io_address  = getSyscallArg(1, buffer) & ~MASK(PAGE_BITS);
    paddr       = pptr_to_paddr((void*)cap_frame_cap_get_capFBasePtr(cap));

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    module_id = cap_io_space_cap_get_capModuleID(io_space);

    if (module_id == asidInvalid) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    slot->cap = cap;

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

void deleteIOPageTable(cap_t io_pt_cap)
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
        }

        lu_ret = lookupIOPDSlot(pd, io_address);
        if (lu_ret.status != EXCEPTION_NONE) {

        }
         
        if (iopde_iopde_pt_ptr_get_address(lu_ret.iopdSlot) != cap_io_page_table_cap_get_capIOPTBasePtr(io_pt_cap)) {
            return;
        }

        iopde_iopde_pt_ptr_new(lu_ret.iopdSlot, 0, 0, 0, 0);
        cleanCacheRange_RAM((word_t)lu_ret.iopdSlot, 
                ((word_t)lu_ret.iopdSlot) + sizeof(iopde_t),
                addrFromPPtr(lu_ret.iopdSlot));

        
        /* TODO flush by address and asid */
        plat_smmu_tlb_flush_all();
        plat_smmu_ptc_flush_all();
    }
}

void unmapIOPage(cap_t cap)
{
    lookupIOPTSlot_ret_t lu_ret;
    iopde_t *pd;
    word_t  io_address;
    uint32_t asid;

    io_address = cap_frame_cap_get_capFMappedAddress(cap);
    asid = cap_frame_cap_get_capFMappedASID(cap);
    pd = (iopde_t *)plat_smmu_lookup_iopd_by_asid(asid);

    if (pd == 0) {
        return;
    }

    lu_ret = lookupIOPTSlot(pd, io_address);

    if (lu_ret.status != EXCEPTION_NONE) {
        return;
    }
    if (iopte_ptr_get_address(lu_ret.ioptSlot) != cap_frame_cap_get_capFBasePtr(cap)) {
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

exception_t
decodeARMIOUnMapInvocation(
    word_t       invLabel,
    uint32_t     length,
    cte_t*       slot,
    cap_t        cap,
    extra_caps_t excaps
)
{
    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t decodeARMIOSpaceInvocation(word_t invLabel, cap_t cap)
{
    userError("IOSpace capability has no invocations");
    current_syscall_error.type = seL4_IllegalOperation;
    return EXCEPTION_SYSCALL_ERROR;
}

