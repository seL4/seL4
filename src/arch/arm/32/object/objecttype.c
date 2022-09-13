/*
 * Copyright 2014, General Dynamics C4 Systems
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
#include <arch/machine/tlb.h>
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#include <arch/object/vcpu.h>
#endif

bool_t Arch_isFrameType(word_t type)
{
    switch (type) {
    case seL4_ARM_SmallPageObject:
        return true;
    case seL4_ARM_LargePageObject:
        return true;
    case seL4_ARM_SectionObject:
        return true;
    case seL4_ARM_SuperSectionObject:
        return true;
    default:
        return false;
    }
}

deriveCap_ret_t Arch_deriveCap(cte_t *slot, cap_t cap)
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

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;
#endif

#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;
#endif

#ifdef CONFIG_TK1_SMMU
    case cap_io_space_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_io_page_table_cap:
        if (cap_io_page_table_cap_get_capIOPTIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving a IOPT cap without an assigned IOASID");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
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
#ifdef CONFIG_TK1_SMMU
            if (isIOSpaceFrameCap(cap)) {
                unmapIOPage(cap);
                break;
            }
#endif

            unmapPage(ARMSmallPage,
                      cap_small_frame_cap_get_capFMappedASID(cap),
                      cap_small_frame_cap_get_capFMappedAddress(cap),
                      (void *)cap_small_frame_cap_get_capFBasePtr(cap));
        }
        break;

    case cap_frame_cap:
        if (cap_frame_cap_get_capFMappedASID(cap)) {
#ifdef CONFIG_KERNEL_LOG_BUFFER
            /* If the last cap to the user-level log buffer frame is being revoked,
             * reset the ksLog so that the kernel doesn't log anymore
             */
            if (unlikely(cap_frame_cap_get_capFSize(cap) == ARMSection)) {
                if (pptr_to_paddr((void *)cap_frame_cap_get_capFBasePtr(cap)) == ksUserLogBuffer) {
                    ksUserLogBuffer = 0;

                    /* Invalidate log page table entries */
                    clearMemory((void *) armKSGlobalLogPT, BIT(seL4_PageTableBits));

                    cleanCacheRange_PoU((pptr_t) &armKSGlobalLogPT[0],
                                        (pptr_t) &armKSGlobalLogPT[0] + BIT(seL4_PageTableBits),
                                        addrFromKPPtr((void *)&armKSGlobalLogPT[0]));

                    for (int idx = 0; idx < BIT(PT_INDEX_BITS); idx++) {
                        invalidateTranslationSingle(KS_LOG_PPTR + (idx << seL4_PageBits));
                    }

                    userError("Log buffer frame is invalidated, kernel can't benchmark anymore");
                }
            }
#endif /* CONFIG_BENCHMARK_KERNEL_LOG_BUFFER */

            unmapPage(cap_frame_cap_get_capFSize(cap),
                      cap_frame_cap_get_capFMappedASID(cap),
                      cap_frame_cap_get_capFMappedAddress(cap),
                      (void *)cap_frame_cap_get_capFBasePtr(cap));
        }
        break;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        if (final) {
            vcpu_finalise(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)));
        }
        break;
#endif

#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        // do nothing
        break;
#endif


#ifdef CONFIG_TK1_SMMU
    case cap_io_space_cap:
        if (final) {
            clearIOPageDirectory(cap);
        }
        break;

    case cap_io_page_table_cap:
        if (final && cap_io_page_table_cap_get_capIOPTIsMapped(cap)) {
            deleteIOPageTable(cap);
        }
        break;
#endif

    default:
        break;
    }

    fc_ret.remainder = cap_null_cap_new();
    fc_ret.cleanupInfo = cap_null_cap_new();
    return fc_ret;
}

bool_t CONST Arch_sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    switch (cap_get_capType(cap_a)) {
    case cap_small_frame_cap:
    case cap_frame_cap:
        if (cap_get_capType(cap_b) == cap_small_frame_cap ||
            cap_get_capType(cap_b) == cap_frame_cap) {
            word_t botA, botB, topA, topB;
            botA = generic_frame_cap_get_capFBasePtr(cap_a);
            botB = generic_frame_cap_get_capFBasePtr(cap_b);
            topA = botA + MASK(pageBitsForSize(generic_frame_cap_get_capFSize(cap_a)));
            topB = botB + MASK(pageBitsForSize(generic_frame_cap_get_capFSize(cap_b))) ;
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

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        if (cap_get_capType(cap_b) == cap_vcpu_cap) {
            return cap_vcpu_cap_get_capVCPUPtr(cap_a) ==
                   cap_vcpu_cap_get_capVCPUPtr(cap_b);
        }
        break;
#endif

#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        if (cap_get_capType(cap_b) == cap_sgi_signal_cap) {
            return (cap_sgi_signal_cap_get_capSGIIRQ(cap_a) ==
                    cap_sgi_signal_cap_get_capSGIIRQ(cap_b) &&
                    cap_sgi_signal_cap_get_capSGITargetMask(cap_a) ==
                    cap_sgi_signal_cap_get_capSGITargetMask(cap_b));
        }
        break;
#endif


#ifdef CONFIG_TK1_SMMU
    case cap_io_space_cap:
        if (cap_get_capType(cap_b) == cap_io_space_cap) {
            return cap_io_space_cap_get_capModuleID(cap_a) ==
                   cap_io_space_cap_get_capModuleID(cap_b);
        }
        break;

    case cap_io_page_table_cap:
        if (cap_get_capType(cap_b) == cap_io_page_table_cap) {
            return cap_io_page_table_cap_get_capIOPTBasePtr(cap_a) ==
                   cap_io_page_table_cap_get_capIOPTBasePtr(cap_b);
        }
        break;
#endif
    }

    return false;
}

bool_t CONST Arch_sameObjectAs(cap_t cap_a, cap_t cap_b)
{
    if (cap_get_capType(cap_a) == cap_small_frame_cap) {
        if (cap_get_capType(cap_b) == cap_small_frame_cap) {
            return ((cap_small_frame_cap_get_capFBasePtr(cap_a) ==
                     cap_small_frame_cap_get_capFBasePtr(cap_b)) &&
                    ((cap_small_frame_cap_get_capFIsDevice(cap_a) == 0) ==
                     (cap_small_frame_cap_get_capFIsDevice(cap_b) == 0)));
        } else if (cap_get_capType(cap_b) == cap_frame_cap) {
            return false;
        }
    }
    if (cap_get_capType(cap_a) == cap_frame_cap) {
        if (cap_get_capType(cap_b) == cap_frame_cap) {
            return ((cap_frame_cap_get_capFBasePtr(cap_a) ==
                     cap_frame_cap_get_capFBasePtr(cap_b)) &&
                    (cap_frame_cap_get_capFSize(cap_a) ==
                     cap_frame_cap_get_capFSize(cap_b)) &&
                    ((cap_frame_cap_get_capFIsDevice(cap_a) == 0) ==
                     (cap_frame_cap_get_capFIsDevice(cap_b) == 0)));
        } else if (cap_get_capType(cap_b) == cap_small_frame_cap) {
            return false;
        }
    }
    return Arch_sameRegionAs(cap_a, cap_b);
}

word_t Arch_getObjectSize(word_t t)
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
        return PTE_SIZE_BITS + PT_INDEX_BITS;
    case seL4_ARM_PageDirectoryObject:
        return PDE_SIZE_BITS + PD_INDEX_BITS;
#ifdef CONFIG_TK1_SMMU
    case seL4_ARM_IOPageTableObject:
        return seL4_IOPageTableBits;
#endif
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case seL4_ARM_VCPUObject:
        return VCPU_SIZE_BITS;
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
    default:
        fail("Invalid object type");
        return 0;
    }
}

cap_t Arch_createObject(object_t t, void *regionBase, word_t userSize, bool_t deviceMemory)
{
    switch (t) {
    case seL4_ARM_SmallPageObject:
        if (deviceMemory) {
            /** AUXUPD: "(True, ptr_retyps 1
                     (Ptr (ptr_val \<acute>regionBase) :: user_data_device_C ptr))" */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSmallPage
                                                    (ptr_val \<acute>regionBase)
                                                    (unat ARMSmallPageBits))" */
        } else {
            /** AUXUPD: "(True, ptr_retyps 1
                     (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSmallPage
                                                    (ptr_val \<acute>regionBase)
                                                    (unat ARMSmallPageBits))" */
        }
        return cap_small_frame_cap_new(
                   ASID_LOW(asidInvalid), VMReadWrite,
                   0, !!deviceMemory,
#ifdef CONFIG_TK1_SMMU
                   0,
#endif
                   ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_LargePageObject:
        if (deviceMemory) {
            /** AUXUPD: "(True, ptr_retyps 16
                     (Ptr (ptr_val \<acute>regionBase) :: user_data_device_C ptr))" */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMLargePage
                                                    (ptr_val \<acute>regionBase)
                                                    (unat ARMLargePageBits))" */
        } else {
            /** AUXUPD: "(True, ptr_retyps 16
                     (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMLargePage
                                                    (ptr_val \<acute>regionBase)
                                                    (unat ARMLargePageBits))" */
        }
        return cap_frame_cap_new(
                   ARMLargePage, ASID_LOW(asidInvalid), VMReadWrite,
                   0, !!deviceMemory, ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_SectionObject:
        if (deviceMemory) {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
            /** AUXUPD: "(True, ptr_retyps 512
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_device_C ptr))" */
#else  /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** AUXUPD: "(True, ptr_retyps 256
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_device_C ptr))" */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSection
                                            (ptr_val \<acute>regionBase)
                                            (unat ARMSectionBits))" */
        } else {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
            /** AUXUPD: "(True, ptr_retyps 512
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
#else  /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** AUXUPD: "(True, ptr_retyps 256
                 (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSection
                                            (ptr_val \<acute>regionBase)
                                            (unat ARMSectionBits))" */
        }
        return cap_frame_cap_new(
                   ARMSection, ASID_LOW(asidInvalid), VMReadWrite,
                   0, !!deviceMemory, ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_SuperSectionObject:
        if (deviceMemory) {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
            /** AUXUPD: "(True, ptr_retyps 8192
                    (Ptr (ptr_val \<acute>regionBase) :: user_data_device_C ptr))" */
#else  /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** AUXUPD: "(True, ptr_retyps 4096
                    (Ptr (ptr_val \<acute>regionBase) :: user_data_device_C ptr))" */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSuperSection
                                                (ptr_val \<acute>regionBase)
                                                (unat ARMSuperSectionBits))" */
        } else {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
            /** AUXUPD: "(True, ptr_retyps 8192
                    (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
#else  /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** AUXUPD: "(True, ptr_retyps 4096
                    (Ptr (ptr_val \<acute>regionBase) :: user_data_C ptr))" */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
            /** GHOSTUPD: "(True, gs_new_frames vmpage_size.ARMSuperSection
                                                (ptr_val \<acute>regionBase)
                                                (unat ARMSuperSectionBits))" */
        }
        return cap_frame_cap_new(
                   ARMSuperSection, ASID_LOW(asidInvalid), VMReadWrite,
                   0, !!deviceMemory, ASID_HIGH(asidInvalid),
                   (word_t)regionBase);

    case seL4_ARM_PageTableObject:
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        /** AUXUPD: "(True, ptr_retyps 1
              (Ptr (ptr_val \<acute>regionBase) :: (pte_C[512]) ptr))" */
#else  /* CONFIG_ARM_HYPERVISOR_SUPPORT */
        /** AUXUPD: "(True, ptr_retyps 1
              (Ptr (ptr_val \<acute>regionBase) :: (pte_C[256]) ptr))" */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

        return cap_page_table_cap_new(false, asidInvalid, 0,
                                      (word_t)regionBase);

    case seL4_ARM_PageDirectoryObject:
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        /** AUXUPD: "(True, ptr_retyps 1
              (Ptr (ptr_val \<acute>regionBase) :: (pde_C[2048]) ptr))" */
#else  /* CONFIG_ARM_HYPERVISOR_SUPPORT */
        /** AUXUPD: "(True, ptr_retyps 1
              (Ptr (ptr_val \<acute>regionBase) :: (pde_C[4096]) ptr))" */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
        copyGlobalMappings((pde_t *)regionBase);
        cleanCacheRange_PoU((word_t)regionBase,
                            (word_t)regionBase + (1 << (PD_INDEX_BITS + PDE_SIZE_BITS)) - 1,
                            addrFromPPtr(regionBase));

        return cap_page_directory_cap_new(false, asidInvalid,
                                          (word_t)regionBase);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case seL4_ARM_VCPUObject:
        /** AUXUPD: "(True, ptr_retyp
          (Ptr (ptr_val \<acute>regionBase) :: vcpu_C ptr))" */
        vcpu_init(VCPU_PTR(regionBase));
        return cap_vcpu_cap_new(VCPU_REF(regionBase));
#endif

#ifdef CONFIG_TK1_SMMU
    case seL4_ARM_IOPageTableObject:
        /* When the untyped was zeroed it was cleaned to the PoU, but the SMMUs
         * typically pull directly from RAM, so we do a futher clean to RAM here */
        cleanCacheRange_RAM((word_t)regionBase,
                            (word_t)regionBase + (1 << seL4_IOPageTableBits) - 1,
                            addrFromPPtr(regionBase));
        return cap_io_page_table_cap_new(0, asidInvalid, (word_t)regionBase, 0);
#endif
    default:
        /*
         * This is a conflation of the haskell error: "Arch.createNewCaps
         * got an API type" and the case where an invalid object type is
         * passed (which is impossible in haskell).
         */
        fail("Arch_createObject got an API type or invalid object type");
    }
}

exception_t Arch_decodeInvocation(word_t invLabel, word_t length, cptr_t cptr,
                                  cte_t *slot, cap_t cap,
                                  bool_t call, word_t *buffer)
{
    /* The C parser cannot handle a switch statement with only a default
     * case. So we need to do some gymnastics to remove the switch if
     * there are no other cases */
#if defined(CONFIG_TK1_SMMU) || defined(CONFIG_ARM_HYPERVISOR_SUPPORT) || (CONFIG_MAX_NUM_NODES == 1)
    switch (cap_get_capType(cap)) {
#ifdef CONFIG_TK1_SMMU
    case cap_io_space_cap:
        return decodeARMIOSpaceInvocation(invLabel, cap);
    case cap_io_page_table_cap:
        return decodeARMIOPTInvocation(invLabel, length, slot, cap, buffer);
#endif
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        return decodeARMVCPUInvocation(invLabel, length, cptr, slot, cap, call, buffer);
#endif /* end of CONFIG_ARM_HYPERVISOR_SUPPORT */
#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        return decodeSGISignalInvocation(invLabel, length, cap, buffer);
#endif /* end of CONFIG_MAX_NUM_NODES == 1 */

    default:
#else
{
#endif
    return decodeARMMMUInvocation(invLabel, length, cptr, slot, cap, call, buffer);
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

