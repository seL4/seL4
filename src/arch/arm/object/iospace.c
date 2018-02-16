/*
 * Copyright 2016, General Dynamics C4 Systems
 * Copyright 2018, DornerWorks
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_DORNERWORKS_GPL)
 */

#include <config.h>

#ifdef CONFIG_ARM_SMMU

#include <api/syscall.h>
#include <machine/io.h>
#include <kernel/thread.h>
#include <arch/api/invocation.h>
#include <arch/object/iospace.h>
#include <arch/model/statedata.h>
#include <object/structures.h>
#include <linker.h>
#include <arch/machine/smmu.h>

BOOT_CODE cap_t
master_iospace_cap(void)
{
    return cap_io_space_cap_new(0);
}

static inline iopde_t iopde_new_invalid(void)
{
   iopde_t pde;

   pde = iopde_new(0, 0);

   return pde;
}

static inline iopde_t iopde_arm_new(word_t paddr)
{
   iopde_t pde;

   pde = iopde_new(
             paddr,
             0x3    /* Coarse */
         );

   return pde;
}

static inline iopte_t iopte_arm_new(bool_t read, bool_t write, word_t paddr)
{
   iopte_t pte;

   int ap = (write ? 0x3 : 0x1);

   pte = iopte_new(
             1,     /* Do not Execute */
             paddr,
             1,     /* Access Flag.  Always 1. */
             0,     /* Non-shareable */
             ap,
             0x5,   /* Normal, Non-cacheable */
             0x3    /* small */
           );

   return pte;
}

static inline iopte_t iopte_new_invalid(void)
{
   iopte_t pte;

   pte = iopte_new(0, 0, 0, 0, 0, 0, 0);

   return pte;
}

typedef struct lookupIOPDSlot_ret {
    exception_t status;
    iopde_t     *iopdSlot;
} lookupIOPDSlot_ret_t;

typedef struct lookupIOPTSlot_ret {
    exception_t status;
    iopte_t     *ioptSlot;
} lookupIOPTSlot_ret_t;

static bool_t isIOPDEValid(iopde_t *iopde)
{
    assert(iopde != 0);

    return (iopde_ptr_get_pdeType(iopde) == 0x3);
}

static bool_t isIOPTEEmpty(iopte_t *iopte)
{
    assert(iopte != 0);

    return (iopte_ptr_get_pteType(iopte) != 0x3);
}


static lookupIOPDSlot_ret_t lookupIOPDSlot(iopde_t *iopd, word_t io_address)
{
    lookupIOPDSlot_ret_t ret;
    uint32_t index = GET_PD_INDEX(io_address);
    ret.status = EXCEPTION_NONE;
    ret.iopdSlot = iopd + index;
    return ret;
}

static lookupIOPTSlot_ret_t lookupIOPTSlot(iopde_t *iopd, word_t io_address)
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

    if (!isIOPDEValid(pd_ret.iopdSlot)) {
        pt_ret.status = EXCEPTION_LOOKUP_FAULT;
        pt_ret.ioptSlot = 0;
        return pt_ret;
    }

    index = GET_PT_INDEX(io_address);
    pt = (iopte_t *)paddr_to_pptr(iopde_ptr_get_address(pd_ret.iopdSlot));

    if (pt == 0) {
        pt_ret.status = EXCEPTION_LOOKUP_FAULT;
        pt_ret.ioptSlot = 0;
        return pt_ret;
    }

    pt_ret.status = EXCEPTION_NONE;
    pt_ret.ioptSlot = pt + index;
    return pt_ret;
}

static exception_t performARMIOPTInvocationMap(cap_t cap, cte_t *slot, iopde_t *iopdSlot,
                                               iopde_t iopde)
{


    *iopdSlot = iopde;
    cleanCacheRange_RAM((word_t)iopdSlot,
                        ((word_t)iopdSlot) + sizeof(iopde_t),
                        addrFromPPtr(iopdSlot));

    plat_smmu_tlb_flush_all();

    slot->cap = cap;
    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}


exception_t decodeARMIOPTInvocation(
    word_t       invLabel,
    uint32_t     length,
    cte_t       *slot,
    cap_t        cap,
    extra_caps_t excaps,
    word_t      *buffer
)
{
    cap_t      io_space;
    word_t     io_address;
    word_t     paddr;
    uint16_t   stream_id;
    uint32_t   asid;
    iopde_t    *pd;
    iopde_t    iopde;
    lookupIOPDSlot_ret_t    lu_ret;

    if (invLabel == ARMIOPageTableUnmap) {
        deleteIOPageTable(slot->cap);
        slot->cap = cap_io_page_table_cap_set_capIOPTIsMapped(slot->cap, 0);

        setThreadState(ksCurThread, ThreadState_Restart);
        return EXCEPTION_NONE;
    }

    if (excaps.excaprefs[0] == NULL || length < 1) {
        userError("IOPTInvocation: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (invLabel != ARMIOPageTableMap) {
        userError("IOPTInvocation: Invalid operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space     = excaps.excaprefs[0]->cap;
    io_address   = getSyscallArg(0, buffer) & ~MASK(PD_INDEX_OFFSET);

    if (cap_io_page_table_cap_get_capIOPTIsMapped(cap)) {
        userError("IOPTMap: Cap already mapped.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        userError("IOPTMap: Invalid IOSpace cap.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    stream_id = cap_io_space_cap_get_capStreamID(io_space);
    asid = plat_smmu_get_asid_by_stream_id(stream_id);
    assert(asid != asidInvalid);

    paddr = pptr_to_paddr((void *)cap_io_page_table_cap_get_capIOPTBasePtr(cap));

    pd = plat_smmu_lookup_iopd_by_asid(asid);

    lu_ret = lookupIOPDSlot(pd, io_address);

    if (isIOPDEValid(lu_ret.iopdSlot)) {
        userError("IOPTMap: Delete first.");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    iopde = iopde_arm_new(paddr);

    cap = cap_io_page_table_cap_set_capIOPTIsMapped(cap, 1);
    cap = cap_io_page_table_cap_set_capIOPTASID(cap, asid);
    cap = cap_io_page_table_cap_set_capIOPTMappedAddress(cap, io_address);

    return performARMIOPTInvocationMap(cap, slot, lu_ret.iopdSlot, iopde);
}

static exception_t performARMIOMapInvocation(cap_t cap, cte_t *slot, iopte_t *ioptSlot,
                                             iopte_t iopte)
{
    *ioptSlot = iopte;
    cleanCacheRange_RAM((word_t)ioptSlot,
                        ((word_t)ioptSlot) + sizeof(iopte_t),
                        addrFromPPtr(ioptSlot));

    plat_smmu_tlb_flush_all();

    slot->cap = cap;

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t decodeARMIOMapInvocation(
    word_t       invLabel,
    uint32_t     length,
    cte_t       *slot,
    cap_t        cap,
    extra_caps_t excaps,
    word_t      *buffer
)
{
    cap_t      io_space;
    paddr_t    io_address;
    paddr_t    paddr;
    uint16_t   stream_id;
    uint32_t   asid;
    iopde_t    *pd;
    iopte_t    iopte;
    vm_rights_t     frame_cap_rights;
    seL4_CapRights_t    dma_cap_rights_mask;
    lookupIOPTSlot_ret_t lu_ret;

    if (excaps.excaprefs[0] == NULL || length < 2) {
        userError("IOMap: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (generic_frame_cap_get_capFSize(cap) != ARMSmallPage) {
        userError("IOMap: Invalid cap type.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (generic_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        userError("IOMap: Frame all ready mapped.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    io_space    = excaps.excaprefs[0]->cap;
    io_address  = getSyscallArg(1, buffer) & ~MASK(PAGE_BITS);
    paddr       = pptr_to_paddr((void *)generic_frame_cap_get_capFBasePtr(cap));

    if (cap_get_capType(io_space) != cap_io_space_cap) {
        userError("IOMap: Invalid IOSpace cap.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    stream_id = cap_io_space_cap_get_capStreamID(io_space);
    asid = plat_smmu_get_asid_by_stream_id(stream_id);
    assert(asid != asidInvalid);

    pd = plat_smmu_lookup_iopd_by_asid(asid);

    lu_ret = lookupIOPTSlot(pd, io_address);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (!isIOPTEEmpty(lu_ret.ioptSlot)) {
        userError("IOMap: Delete first.");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }
    frame_cap_rights = generic_frame_cap_get_capFVMRights(cap);
    dma_cap_rights_mask = rightsFromWord(getSyscallArg(0, buffer));

    if ((frame_cap_rights == VMReadOnly) && seL4_CapRights_get_capAllowRead(dma_cap_rights_mask)) {
        iopte = iopte_arm_new(true, false, paddr);
    } else if (frame_cap_rights == VMReadWrite) {
        if (seL4_CapRights_get_capAllowRead(dma_cap_rights_mask) &&
            !seL4_CapRights_get_capAllowWrite(dma_cap_rights_mask)) {
            iopte = iopte_arm_new(true, false, paddr);
        } else if (!seL4_CapRights_get_capAllowRead(dma_cap_rights_mask) &&
                   seL4_CapRights_get_capAllowWrite(dma_cap_rights_mask)) {
            iopte = iopte_arm_new(false, true, paddr);
        } else if (seL4_CapRights_get_capAllowRead(dma_cap_rights_mask) &&
                   seL4_CapRights_get_capAllowWrite(dma_cap_rights_mask)) {
            iopte = iopte_arm_new(true, true, paddr);
        } else {
            userError("IOMap: Invalid argument.");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

    } else {
        /* VMKernelOnly */
        userError("IOMap: Invalid argument.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cap = generic_frame_cap_set_capFIsIOSpace(cap, 1);
    cap = generic_frame_cap_set_capFMappedAddress(cap, asid, io_address);

    return performARMIOMapInvocation(cap, slot, lu_ret.ioptSlot, iopte);
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
        assert(asid != asidInvalid);
        pd = plat_smmu_lookup_iopd_by_asid(asid);
        io_address = cap_io_page_table_cap_get_capIOPTMappedAddress(io_pt_cap);

        lu_ret = lookupIOPDSlot(pd, io_address);
        if (lu_ret.status != EXCEPTION_NONE) {
            return;
        }

        if (isIOPDEValid(lu_ret.iopdSlot) &&
            iopde_ptr_get_address(lu_ret.iopdSlot) != (pptr_to_paddr((void *)cap_io_page_table_cap_get_capIOPTBasePtr(
                                                                         io_pt_cap)))) {
            return;
        }

        *lu_ret.iopdSlot = iopde_new_invalid();
        cleanCacheRange_RAM((word_t)lu_ret.iopdSlot,
                            ((word_t)lu_ret.iopdSlot) + sizeof(iopde_t),
                            addrFromPPtr(lu_ret.iopdSlot));


        /* nice to have: flush by address and asid */
        plat_smmu_tlb_flush_all();
    }
}

void unmapIOPage(cap_t cap)
{
    lookupIOPTSlot_ret_t lu_ret;
    iopde_t *pd;
    word_t  io_address;
    uint32_t asid;

    io_address = generic_frame_cap_get_capFMappedAddress(cap);
    asid = generic_frame_cap_get_capFMappedASID(cap);
    assert(asid != asidInvalid);
    pd = plat_smmu_lookup_iopd_by_asid(asid);

    lu_ret = lookupIOPTSlot(pd, io_address);

    if (lu_ret.status != EXCEPTION_NONE) {
        return;
    }
    if (iopte_ptr_get_address(lu_ret.ioptSlot) != pptr_to_paddr((void *)generic_frame_cap_get_capFBasePtr(cap))) {
        return;
    }

    *lu_ret.ioptSlot = iopte_new_invalid();
    cleanCacheRange_RAM((word_t)lu_ret.ioptSlot,
                        ((word_t)lu_ret.ioptSlot) + sizeof(iopte_t),
                        addrFromPPtr(lu_ret.ioptSlot));

    plat_smmu_tlb_flush_all();

    return;
}

void clearIOPageDirectory(cap_t cap)
{
    iopde_t  *pd;
    uint16_t stream_id = cap_io_space_cap_get_capStreamID(cap);
    uint32_t asid;
    word_t   size = BIT(PD_INDEX_BITS);

    asid = plat_smmu_get_asid_by_stream_id(stream_id);
    assert(asid != asidInvalid);

    pd = plat_smmu_lookup_iopd_by_asid(asid);

    memset((void *)pd, 0, size);
    cleanCacheRange_RAM((word_t)pd, (word_t)pd + size, addrFromPPtr(pd));

    plat_smmu_tlb_flush_all();

    return;
}

exception_t performPageInvocationUnmapIO(
    cap_t        cap,
    cte_t       *slot
)
{
    unmapIOPage(slot->cap);
    slot->cap = generic_frame_cap_set_capFMappedAddress(slot->cap, asidInvalid, 0);
    slot->cap = generic_frame_cap_set_capFIsIOSpace(slot->cap, 0);

    return EXCEPTION_NONE;
}

exception_t decodeARMIOSpaceInvocation(word_t invLabel, cap_t cap)
{
    userError("IOSpace capability has no invocations");
    current_syscall_error.type = seL4_IllegalOperation;
    return EXCEPTION_SYSCALL_ERROR;
}
#endif /* end of CONFIG_ARM_SMMU */
