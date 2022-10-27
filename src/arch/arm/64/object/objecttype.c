/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <kernel/vspace.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <arch/model/statedata.h>
#include <arch/object/objecttype.h>

bool_t Arch_isFrameType(word_t type)
{
    switch (type) {
    case seL4_ARM_SmallPageObject:
        return true;
    case seL4_ARM_LargePageObject:
        return true;
    case seL4_ARM_HugePageObject:
        return true;
    default:
        return false;
    }
}

deriveCap_ret_t Arch_deriveCap(cte_t *slot, cap_t cap)
{
    deriveCap_ret_t ret;

    switch (cap_get_capType(cap)) {
    case cap_page_global_directory_cap:
        if (cap_page_global_directory_cap_get_capPGDIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving a PDG cap without an assigned ASID");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

    case cap_page_upper_directory_cap:
        if (cap_page_upper_directory_cap_get_capPUDIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving a PUD cap without an assigned ASID");
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

    case cap_page_table_cap:
        if (cap_page_table_cap_get_capPTIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving a PT cap without an assigned ASID");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
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

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;
#endif
#ifdef CONFIG_ARM_SMMU
    case cap_sid_control_cap:
    case cap_cb_control_cap:
        ret.cap = cap_null_cap_new();
        ret.status = EXCEPTION_NONE;
        return ret;
    case cap_sid_cap:
    case cap_cb_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;
#endif
    default:
        /* This assert has no equivalent in haskell,
         * as the options are restricted by type */
        fail("Invalid arch cap");
    }
}

cap_t CONST Arch_updateCapData(bool_t preserve, word_t data, cap_t cap)
{
    return cap;
}

cap_t CONST Arch_maskCapRights(seL4_CapRights_t cap_rights_mask, cap_t cap)
{
    if (cap_get_capType(cap) == cap_frame_cap) {
        vm_rights_t vm_rights;

        vm_rights = vmRightsFromWord(cap_frame_cap_get_capFVMRights(cap));
        vm_rights = maskVMRights(vm_rights, cap_rights_mask);

        return cap_frame_cap_set_capFVMRights(cap, wordFromVMRights(vm_rights));
    } else {
        return cap;
    }
}

finaliseCap_ret_t Arch_finaliseCap(cap_t cap, bool_t final)
{
    finaliseCap_ret_t fc_ret;

    switch (cap_get_capType(cap)) {
    case cap_asid_pool_cap:
        if (final) {
            deleteASIDPool(cap_asid_pool_cap_get_capASIDBase(cap),
                           ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap)));
        }
        break;

    case cap_page_global_directory_cap:
#ifdef CONFIG_ARM_SMMU
        if (cap_page_global_directory_cap_get_capPGDMappedCB(cap) != CB_INVALID) {
            smmu_cb_delete_vspace(cap_page_global_directory_cap_get_capPGDMappedCB(cap),
                                  cap_page_global_directory_cap_get_capPGDMappedASID(cap));
        }
#endif
        if (final && cap_page_global_directory_cap_get_capPGDIsMapped(cap)) {
            deleteASID(cap_page_global_directory_cap_get_capPGDMappedASID(cap),
                       VSPACE_PTR(cap_page_global_directory_cap_get_capPGDBasePtr(cap)));
        }
        break;

    case cap_page_upper_directory_cap:
#ifdef AARCH64_VSPACE_S2_START_L1
#ifdef CONFIG_ARM_SMMU
        if (cap_page_upper_directory_cap_get_capPGDMappedCB(cap) != CB_INVALID) {
            smmu_cb_delete_vspace(cap_page_upper_directory_cap_get_capPUDMappedCB(cap),
                                  cap_page_upper_directory_cap_get_capPUDMappedASID(cap));
        }
#endif
        if (final && cap_page_upper_directory_cap_get_capPUDIsMapped(cap)) {
            deleteASID(cap_page_upper_directory_cap_get_capPUDMappedASID(cap),
                       PUDE_PTR(cap_page_upper_directory_cap_get_capPUDBasePtr(cap)));
        }
#else
        if (final && cap_page_upper_directory_cap_get_capPUDIsMapped(cap)) {
            unmapPageUpperDirectory(cap_page_upper_directory_cap_get_capPUDMappedASID(cap),
                                    cap_page_upper_directory_cap_get_capPUDMappedAddress(cap),
                                    PUDE_PTR(cap_page_upper_directory_cap_get_capPUDBasePtr(cap)));
        }

#endif
        break;

    case cap_page_directory_cap:
        if (final && cap_page_directory_cap_get_capPDIsMapped(cap)) {
            unmapPageDirectory(cap_page_directory_cap_get_capPDMappedASID(cap),
                               cap_page_directory_cap_get_capPDMappedAddress(cap),
                               PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap)));
        }
        break;

    case cap_page_table_cap:
        if (final && cap_page_table_cap_get_capPTIsMapped(cap)) {
            unmapPageTable(cap_page_table_cap_get_capPTMappedASID(cap),
                           cap_page_table_cap_get_capPTMappedAddress(cap),
                           PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
        }
        break;

    case cap_frame_cap:
        if (cap_frame_cap_get_capFMappedASID(cap)) {
            unmapPage(cap_frame_cap_get_capFSize(cap),
                      cap_frame_cap_get_capFMappedASID(cap),
                      cap_frame_cap_get_capFMappedAddress(cap),
                      cap_frame_cap_get_capFBasePtr(cap));
        }
        break;
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        if (final) {
            vcpu_finalise(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)));
        }
        break;
#endif
#ifdef CONFIG_ARM_SMMU
    case cap_cb_cap:
        if (cap_cb_cap_get_capBindSID(cap) != SID_INVALID) {
            smmu_sid_unbind(cap_cb_cap_get_capBindSID(cap));
        }
        if (final) {
            smmu_delete_cb(cap);
        }
        break;
    case cap_sid_cap:
        if (final) {
            smmu_delete_sid(cap);
        }
        break;
#endif
    }

    fc_ret.remainder = cap_null_cap_new();
    fc_ret.cleanupInfo = cap_null_cap_new();
    return fc_ret;
}

bool_t CONST Arch_sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    switch (cap_get_capType(cap_a)) {
    case cap_frame_cap:
        if (cap_get_capType(cap_b) == cap_frame_cap) {

            word_t botA, botB, topA, topB;
            botA = cap_frame_cap_get_capFBasePtr(cap_a);
            botB = cap_frame_cap_get_capFBasePtr(cap_b);
            topA = botA + MASK(pageBitsForSize(cap_frame_cap_get_capFSize(cap_a)));
            topB = botB + MASK(pageBitsForSize(cap_frame_cap_get_capFSize(cap_b))) ;
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

    case cap_page_upper_directory_cap:
        if (cap_get_capType(cap_b) == cap_page_upper_directory_cap) {
            return cap_page_upper_directory_cap_get_capPUDBasePtr(cap_a) ==
                   cap_page_upper_directory_cap_get_capPUDBasePtr(cap_b);
        }
        break;

    case cap_page_global_directory_cap:
        if (cap_get_capType(cap_b) == cap_page_global_directory_cap) {
            return cap_page_global_directory_cap_get_capPGDBasePtr(cap_a) ==
                   cap_page_global_directory_cap_get_capPGDBasePtr(cap_b);
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

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        if (cap_get_capType(cap_b) == cap_vcpu_cap) {
            return cap_vcpu_cap_get_capVCPUPtr(cap_a) ==
                   cap_vcpu_cap_get_capVCPUPtr(cap_b);
        }
        break;
#endif
#ifdef CONFIG_ARM_SMMU
    case cap_sid_control_cap:
        if (cap_get_capType(cap_b) == cap_sid_control_cap ||
            cap_get_capType(cap_b) == cap_sid_cap) {
            return true;
        }
        break;
    case cap_cb_control_cap:
        if (cap_get_capType(cap_b) == cap_cb_control_cap ||
            cap_get_capType(cap_b) == cap_cb_cap) {
            return true;
        }
        break;
    case cap_sid_cap:
        if (cap_get_capType(cap_b) == cap_sid_cap) {
            return cap_sid_cap_get_capSID(cap_a) ==
                   cap_sid_cap_get_capSID(cap_b);
        }
        break;
    case cap_cb_cap:
        if (cap_get_capType(cap_b) == cap_cb_cap) {
            return cap_cb_cap_get_capCB(cap_a) ==
                   cap_cb_cap_get_capCB(cap_b);
        }
        break;
#endif
    }
    return false;
}

bool_t CONST Arch_sameObjectAs(cap_t cap_a, cap_t cap_b)
{
    if (cap_get_capType(cap_a) == cap_frame_cap) {
        if (cap_get_capType(cap_b) == cap_frame_cap) {
            return ((cap_frame_cap_get_capFBasePtr(cap_a) ==
                     cap_frame_cap_get_capFBasePtr(cap_b)) &&
                    (cap_frame_cap_get_capFSize(cap_a) ==
                     cap_frame_cap_get_capFSize(cap_b)) &&
                    ((cap_frame_cap_get_capFIsDevice(cap_a) == 0) ==
                     (cap_frame_cap_get_capFIsDevice(cap_b) == 0)));
        }
    }
#ifdef CONFIG_ARM_SMMU
    if (cap_get_capType(cap_a) == cap_sid_control_cap &&
        cap_get_capType(cap_b) == cap_sid_cap) {
        return false;
    }
    if (cap_get_capType(cap_a) == cap_cb_control_cap &&
        cap_get_capType(cap_b) == cap_cb_cap) {
        return false;
    }
#endif
    return Arch_sameRegionAs(cap_a, cap_b);
}

word_t Arch_getObjectSize(word_t t)
{
    switch (t) {
    case seL4_ARM_SmallPageObject:
        return ARMSmallPageBits;
    case seL4_ARM_LargePageObject:
        return ARMLargePageBits;
    case seL4_ARM_HugePageObject:
        return ARMHugePageBits;
    case seL4_ARM_PageTableObject:
        return seL4_PageTableBits;
    case seL4_ARM_PageDirectoryObject:
        return seL4_PageDirBits;
    case seL4_ARM_PageUpperDirectoryObject:
        return seL4_PUDBits;
#ifndef AARCH64_VSPACE_S2_START_L1
    case seL4_ARM_PageGlobalDirectoryObject:
        return seL4_PGDBits;
#endif
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case seL4_ARM_VCPUObject:
        return VCPU_SIZE_BITS;
#endif
    default:
        fail("Invalid object type");
        return 0;
    }
}

cap_t Arch_createObject(object_t t, void *regionBase, word_t userSize, bool_t deviceMemory)
{
    switch (t) {
    case seL4_ARM_SmallPageObject:
        return cap_frame_cap_new(
                   asidInvalid,           /* capFMappedASID */
                   (word_t)regionBase,    /* capFBasePtr */
                   ARMSmallPage,          /* capFSize */
                   0,                     /* capFMappedAddress */
                   VMReadWrite,           /* capFVMRights */
                   !!deviceMemory         /* capFIsDevice */
               );

    case seL4_ARM_LargePageObject:
        return cap_frame_cap_new(
                   asidInvalid,           /* capFMappedASID */
                   (word_t)regionBase,    /* capFBasePtr */
                   ARMLargePage,          /* capFSize */
                   0,                     /* capFMappedAddress */
                   VMReadWrite,           /* capFVMRights */
                   !!deviceMemory         /* capFIsDevice */
               );

    case seL4_ARM_HugePageObject:
        return cap_frame_cap_new(
                   asidInvalid,           /* capFMappedASID */
                   (word_t)regionBase,    /* capFBasePtr */
                   ARMHugePage,           /* capFSize */
                   0,                     /* capFMappedAddress */
                   VMReadWrite,           /* capFVMRights */
                   !!deviceMemory         /* capFIsDevice */
               );
#ifndef AARCH64_VSPACE_S2_START_L1
    case seL4_ARM_PageGlobalDirectoryObject:
#ifdef CONFIG_ARM_SMMU

        return cap_page_global_directory_cap_new(
                   asidInvalid,           /* capPGDMappedASID   */
                   (word_t)regionBase,    /* capPGDBasePtr      */
                   0,                     /* capPGDIsMapped     */
                   CB_INVALID             /* capPGDMappedCB     */
               );
#else

        return cap_page_global_directory_cap_new(
                   asidInvalid,           /* capPGDMappedASID   */
                   (word_t)regionBase,    /* capPGDBasePtr      */
                   0                      /* capPGDIsMapped     */
               );
#endif /*!CONFIG_ARM_SMMU*/
#endif /*!AARCH64_VSPACE_S2_START_L1*/
    case seL4_ARM_PageUpperDirectoryObject:
        return cap_page_upper_directory_cap_new(
                   asidInvalid,           /* capPUDMappedASID    */
                   (word_t)regionBase,    /* capPUDBasePtr       */
                   0,                     /* capPUDIsMapped      */
                   0                      /* capPUDMappedAddress */
               );

    case seL4_ARM_PageDirectoryObject:
        return cap_page_directory_cap_new(
                   asidInvalid,           /* capPDMappedASID    */
                   (word_t)regionBase,    /* capPDBasePtr       */
                   0,                     /* capPDIsMapped      */
                   0                      /* capPDMappedAddress */
               );

    case seL4_ARM_PageTableObject:
        return cap_page_table_cap_new(
                   asidInvalid,           /* capPTMappedASID    */
                   (word_t)regionBase,    /* capPTBasePtr       */
                   0,                     /* capPTIsMapped      */
                   0                      /* capPTMappedAddress */
               );

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case seL4_ARM_VCPUObject:
        /** AUXUPD: "(True, ptr_retyp
          (Ptr (ptr_val \<acute>regionBase) :: vcpu_C ptr))" */
        vcpu_init(VCPU_PTR(regionBase));
        return cap_vcpu_cap_new(VCPU_REF(regionBase));
#endif

    default:
        fail("Arch_createObject got an API type or invalid object type");
    }
}

exception_t Arch_decodeInvocation(word_t label, word_t length, cptr_t cptr,
                                  cte_t *slot, cap_t cap,
                                  bool_t call, word_t *buffer)
{

    /* The C parser cannot handle a switch statement with only a default
     * case. So we need to do some gymnastics to remove the switch if
     * there are no other cases */
#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) || defined(CONFIG_ARM_SMMU)
    switch (cap_get_capType(cap)) {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        return decodeARMVCPUInvocation(label, length, cptr, slot, cap, call, buffer);
#endif /* end of CONFIG_ARM_HYPERVISOR_SUPPORT */
#ifdef CONFIG_ARM_SMMU
    case cap_sid_control_cap:
        return decodeARMSIDControlInvocation(label, length, cptr, slot, cap, call, buffer);
    case cap_sid_cap:
        return decodeARMSIDInvocation(label, length, cptr, slot, cap, call, buffer);
    case cap_cb_control_cap:
        return decodeARMCBControlInvocation(label, length, cptr, slot, cap, call, buffer);
    case cap_cb_cap:
        return decodeARMCBInvocation(label, length, cptr, slot, cap, call, buffer);
#endif /*CONFIG_ARM_SMMU*/
    default:
#else
{
#endif
    return decodeARMMMUInvocation(label, length, cptr, slot, cap, call, buffer);
}
}

void
Arch_prepareThreadDelete(tcb_t * thread) {
#ifdef CONFIG_HAVE_FPU
    fpuThreadDelete(thread);
#endif

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    if (thread->tcbArch.tcbVCPU) {
        dissociateVCPUTCB(thread->tcbArch.tcbVCPU, thread);
    }
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
}
