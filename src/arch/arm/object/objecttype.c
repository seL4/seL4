/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <api/failures.h>
#include <kernel/vspace.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <arch/model/statedata.h>
#include <arch/object/objecttype.h>

deriveCap_ret_t
Arch_deriveCap(cte_t *slot, cap_t cap)
{
    deriveCap_ret_t ret;

    switch (cap_get_capType(cap)) {
    case cap_page_table_cap:
        if (cap_page_table_cap_get_capPTIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving an unmapped PT cap");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

    case cap_page_directory_cap:
        if (cap_page_directory_cap_get_capPDIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving a PD cap without an assigned ASID");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

        /* This is a deviation from haskell, which has only
         * one frame cap type on ARM */
    case cap_small_frame_cap:
        ret.cap = cap_small_frame_cap_set_capFMappedASID(cap, asidInvalid);
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_frame_cap:
        ret.cap = cap_frame_cap_set_capFMappedASID(cap, asidInvalid);
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_asid_control_cap:
    case cap_asid_pool_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

    default:
        /* This assert has no equivalent in haskell,
         * as the options are restricted by type */
        fail("Invalid arch cap");
    }
}

cap_t CONST
Arch_updateCapData(bool_t preserve, word_t data, cap_t cap)
{
    return cap;
}

cap_t CONST
Arch_maskCapRights(cap_rights_t cap_rights_mask, cap_t cap)
{
    if (cap_get_capType(cap) == cap_small_frame_cap) {
        vm_rights_t vm_rights;

        vm_rights = vmRightsFromWord(
                        cap_small_frame_cap_get_capFVMRights(cap));
        vm_rights = maskVMRights(vm_rights, cap_rights_mask);
        return cap_small_frame_cap_set_capFVMRights(cap,
                                                    wordFromVMRights(vm_rights));
    } else if (cap_get_capType(cap) == cap_frame_cap) {
        vm_rights_t vm_rights;

        vm_rights = vmRightsFromWord(
                        cap_frame_cap_get_capFVMRights(cap));
        vm_rights = maskVMRights(vm_rights, cap_rights_mask);
        return cap_frame_cap_set_capFVMRights(cap,
                                              wordFromVMRights(vm_rights));
    } else {
        return cap;
    }
}

cap_t
Arch_finaliseCap(cap_t cap, bool_t final)
{
    switch (cap_get_capType(cap)) {
    case cap_asid_pool_cap:
        if (final) {
            deleteASIDPool(cap_asid_pool_cap_get_capASIDBase(cap),
                           ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap)));
        }
        break;

    case cap_page_directory_cap:
        if (final && cap_page_directory_cap_get_capPDIsMapped(cap)) {
            deleteASID(cap_page_directory_cap_get_capPDMappedASID(cap),
                       PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap)));
        }
        break;

    case cap_page_table_cap:
        if (final && cap_page_table_cap_get_capPTIsMapped(cap)) {
            unmapPageTable(
                cap_page_table_cap_get_capPTMappedASID(cap),
                cap_page_table_cap_get_capPTMappedAddress(cap),
                PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
        }
        break;

    case cap_small_frame_cap:
        if (cap_small_frame_cap_get_capFMappedASID(cap)) {
            unmapPage(ARMSmallPage,
                      cap_small_frame_cap_get_capFMappedASID(cap),
                      cap_small_frame_cap_get_capFMappedAddress(cap),
                      (void *)cap_small_frame_cap_get_capFBasePtr(cap));
        }
        break;

    case cap_frame_cap:
        if (cap_frame_cap_get_capFMappedASID(cap)) {
            unmapPage(cap_frame_cap_get_capFSize(cap),
                      cap_frame_cap_get_capFMappedASID(cap),
                      cap_frame_cap_get_capFMappedAddress(cap),
                      (void *)cap_frame_cap_get_capFBasePtr(cap));
        }
        break;
    }

    return cap_null_cap_new();
}

static cap_t CONST
resetMemMapping(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_small_frame_cap:
        return cap_small_frame_cap_set_capFMappedASID(cap, asidInvalid);
    case cap_frame_cap:
        return cap_frame_cap_set_capFMappedASID(cap, asidInvalid);
    case cap_page_table_cap:
        /* We don't need to worry about clearing ASID and Address here, only whether it is mapped */
        return cap_page_table_cap_set_capPTIsMapped(cap, 0);
    case cap_page_directory_cap:
        /* We don't need to worry about clearing ASID and Address here, only whether it is mapped */
        return cap_page_directory_cap_set_capPDIsMapped(cap, 0);
    }

    return cap;
}

cap_t
Arch_recycleCap(bool_t is_final, cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_small_frame_cap:
    case cap_frame_cap:
        clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        Arch_finaliseCap(cap, is_final);
        return resetMemMapping(cap);

    case cap_page_table_cap: {
        pte_t *pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap));
        clearMemory((void *)pt, cap_get_capSizeBits(cap));
        if (cap_page_table_cap_get_capPTIsMapped(cap)) {
            asid_t asid = cap_page_table_cap_get_capPTMappedASID(cap);
            vptr_t vptr = cap_page_table_cap_get_capPTMappedAddress(cap);
            if (pageTableMapped (asid, vptr, pt) != NULL) {
                invalidateTLBByASID(asid);
            }
        }

        Arch_finaliseCap(cap, is_final);
        if (is_final) {
            return resetMemMapping(cap);
        }

        return cap;
    }
    case cap_page_directory_cap: {
        pde_t *pdPtr
            = PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

        memzero(pdPtr, (kernelBase >> ARMSectionBits) << PDE_SIZE_BITS);
        cleanCacheRange_PoU((word_t)pdPtr,
                            ((word_t)pdPtr) + (1 << PD_SIZE_BITS) - 1,
                            addrFromPPtr(pdPtr));

        if (cap_page_directory_cap_get_capPDIsMapped(cap)) {
            findPDForASID_ret_t find_ret;
            asid_t asid = cap_page_directory_cap_get_capPDMappedASID(cap);

            find_ret = findPDForASID (asid);
            if (find_ret.status == EXCEPTION_NONE && find_ret.pd == pdPtr) {
                invalidateTLBByASID(asid);
            }
        }

        Arch_finaliseCap(cap, is_final);
        if (is_final) {
            return resetMemMapping(cap);
        }

        return cap;
    }

    case cap_asid_control_cap:
        return cap;
    case cap_asid_pool_cap: {
        word_t base = cap_asid_pool_cap_get_capASIDBase(cap);
        asid_pool_t* ptr =
            ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));
        if (armKSASIDTable[base >> asidLowBits] == ptr) {
            deleteASIDPool(base, ptr);
            memzero(ptr, sizeof(pde_t*) << asidLowBits);
            armKSASIDTable[base >> asidLowBits] = ptr;
        }
        return cap;
    }
    default:
        fail("Arch_recycleCap: invalid cap type");
    }

    return cap_null_cap_new();
}

bool_t CONST
Arch_hasRecycleRights(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_small_frame_cap:
    case cap_frame_cap:
        return generic_frame_cap_get_capFVMRights(cap) == VMReadWrite;

    default:
        return true;
    }
}


bool_t CONST
Arch_sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    switch (cap_get_capType(cap_a)) {
    case cap_small_frame_cap:
    case cap_frame_cap:
        if (cap_get_capType(cap_b) == cap_small_frame_cap ||
                cap_get_capType(cap_b) == cap_frame_cap) {
            word_t botA, botB, topA, topB;
            botA = generic_frame_cap_get_capFBasePtr(cap_a);
            botB = generic_frame_cap_get_capFBasePtr(cap_b);
            topA = botA + MASK (pageBitsForSize(generic_frame_cap_get_capFSize(cap_a)));
            topB = botB + MASK (pageBitsForSize(generic_frame_cap_get_capFSize(cap_b))) ;
            return ((botA <= botB) && (topA >= topB) && (botB <= topB));
        }
        break;

    case cap_page_table_cap:
        if (cap_get_capType(cap_b) == cap_page_table_cap) {
            return cap_page_table_cap_get_capPTBasePtr(cap_a) ==
                   cap_page_table_cap_get_capPTBasePtr(cap_b);
        }
        break;

    case cap_page_directory_cap:
        if (cap_get_capType(cap_b) == cap_page_directory_cap) {
            return cap_page_directory_cap_get_capPDBasePtr(cap_a) ==
                   cap_page_directory_cap_get_capPDBasePtr(cap_b);
        }
        break;

    case cap_asid_control_cap:
        if (cap_get_capType(cap_b) == cap_asid_control_cap) {
            return true;
        }
        break;

    case cap_asid_pool_cap:
        if (cap_get_capType(cap_b) == cap_asid_pool_cap) {
            return cap_asid_pool_cap_get_capASIDPool(cap_a) ==
                   cap_asid_pool_cap_get_capASIDPool(cap_b);
        }
        break;
    }

    return false;
}


bool_t CONST
Arch_sameObjectAs(cap_t cap_a, cap_t cap_b)
{
    if (cap_get_capType(cap_a) == cap_small_frame_cap) {
        if (cap_get_capType(cap_b) == cap_small_frame_cap) {
            return (cap_small_frame_cap_get_capFBasePtr(cap_a) ==
                    cap_small_frame_cap_get_capFBasePtr(cap_b));
        } else if (cap_get_capType(cap_b) == cap_frame_cap) {
            return false;
        }
    }
    if (cap_get_capType(cap_a) == cap_frame_cap) {
        if (cap_get_capType(cap_b) == cap_frame_cap) {
            return ((cap_frame_cap_get_capFBasePtr(cap_a) ==
                     cap_frame_cap_get_capFBasePtr(cap_b)) &&
                    (cap_frame_cap_get_capFSize(cap_a) ==
                     cap_frame_cap_get_capFSize(cap_b)));
        } else if (cap_get_capType(cap_b) == cap_small_frame_cap) {
            return false;
        }
    }
    return Arch_sameRegionAs(cap_a, cap_b);
}

word_t
Arch_getObjectSize(word_t t)
{
    switch (t) {
    case seL4_ARM_SmallPageObject:
        return ARMSmallPageBits;
    case seL4_ARM_LargePageObject:
        return ARMLargePageBits;
    case seL4_ARM_SectionObject:
        return ARMSectionBits;
    case seL4_ARM_SuperSectionObject:
        return ARMSuperSectionBits;
    case seL4_ARM_PageTableObject:
        return PTE_SIZE_BITS + PT_BITS;
    case seL4_ARM_PageDirectoryObject:
        return PDE_SIZE_BITS + PD_BITS;
    default:
        fail("Invalid object type");
        return 0;
    }
}

cap_t
Arch_createObject(object_t t, void *regionBase, word_t userSize)
{
    switch (t) {
    case seL4_ARM_SmallPageObject:
        memzero(regionBase, 1 << ARMSmallPageBits);
        /** AUXUPD: "(True, ptr_retyps 1
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
        /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSmallPage
                                            (ptr_val \<acute>regionBase)
                                            (unat ARMSmallPageBits))" */
        cleanCacheRange_PoU((word_t)regionBase,
                            (word_t)regionBase + (1 << ARMSmallPageBits) - 1,
                            addrFromPPtr(regionBase));

        return cap_small_frame_cap_new(
                   ASID_LOW(asidInvalid), VMReadWrite,
                   0, ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_LargePageObject:
        memzero(regionBase, 1 << ARMLargePageBits);
        /** AUXUPD: "(True, ptr_retyps 16
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
        /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMLargePage
                                            (ptr_val \<acute>regionBase)
                                            (unat ARMLargePageBits))" */
        cleanCacheRange_PoU((word_t)regionBase,
                            (word_t)regionBase + (1 << ARMLargePageBits) - 1,
                            addrFromPPtr(regionBase));

        return cap_frame_cap_new(
                   ARMLargePage, ASID_LOW(asidInvalid), VMReadWrite,
                   0, ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_SectionObject:
        memzero(regionBase, 1 << ARMSectionBits);
        /** AUXUPD: "(True, ptr_retyps 256
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
        /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSection
                                            (ptr_val \<acute>regionBase)
                                            (unat ARMSectionBits))" */
        cleanCacheRange_PoU((word_t)regionBase,
                            (word_t)regionBase + (1 << ARMSectionBits) - 1,
                            addrFromPPtr(regionBase));

        return cap_frame_cap_new(
                   ARMSection, ASID_LOW(asidInvalid), VMReadWrite,
                   0, ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_SuperSectionObject:
        memzero(regionBase, 1 << ARMSuperSectionBits);
        /** AUXUPD: "(True, ptr_retyps 4096
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
        /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSuperSection
                                            (ptr_val \<acute>regionBase)
                                            (unat ARMSuperSectionBits))" */
        cleanCacheRange_PoU((word_t)regionBase,
                            (word_t)regionBase + (1 << ARMSuperSectionBits) - 1,
                            addrFromPPtr(regionBase));

        return cap_frame_cap_new(
                   ARMSuperSection, ASID_LOW(asidInvalid), VMReadWrite,
                   0, ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_PageTableObject:
        memzero(regionBase, 1 << (PTE_SIZE_BITS + PT_BITS));
        /** AUXUPD: "(True, ptr_retyps 256
              (Ptr (ptr_val \<acute>regionBase) :: pte_C ptr))" */
        cleanCacheRange_PoU((word_t)regionBase,
                            (word_t)regionBase + (1 << (PT_BITS + PTE_SIZE_BITS)) - 1,
                            addrFromPPtr(regionBase));

        return cap_page_table_cap_new(false, asidInvalid, 0,
                                      (word_t)regionBase);

    case seL4_ARM_PageDirectoryObject:
        memzero(regionBase, 1 << (PDE_SIZE_BITS + PD_BITS));
        /** AUXUPD: "(True, ptr_retyps 4096
              (Ptr (ptr_val \<acute>regionBase) :: pde_C ptr))" */
        copyGlobalMappings((pde_t *)regionBase);
        cleanCacheRange_PoU((word_t)regionBase,
                            (word_t)regionBase + (1 << (PD_BITS + PDE_SIZE_BITS)) - 1,
                            addrFromPPtr(regionBase));

        return cap_page_directory_cap_new(false, asidInvalid,
                                          (word_t)regionBase);

    default:
        /*
         * This is a conflation of the haskell error: "Arch.createNewCaps
         * got an API type" and the case where an invalid object type is
         * passed (which is impossible in haskell).
         */
        fail("Arch_createObject got an API type or invalid object type");
    }
}

exception_t
Arch_decodeInvocation(word_t label, unsigned int length, cptr_t cptr,
                      cte_t *slot, cap_t cap, extra_caps_t extraCaps,
                      word_t *buffer)
{
    return decodeARMMMUInvocation(label, length, cptr, slot, cap, extraCaps, buffer);
}

void
Arch_prepareThreadDelete(tcb_t *thread)
{
    /* No action required on ARM. */
}

