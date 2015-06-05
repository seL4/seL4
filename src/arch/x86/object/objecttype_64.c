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
#include <types.h>
#include <api/failures.h>
#include <kernel/vspace.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <arch/model/statedata.h>
#include <arch/machine/fpu.h>
#include <arch/object/objecttype.h>
#include <arch/object/ioport.h>
#include <plat/machine/pci.h>

#include <arch/object/iospace.h>
#include <plat/machine/intel-vtd.h>

deriveCap_ret_t Mode_deriveCap(cte_t* slot, cap_t cap)
{
    deriveCap_ret_t ret;

    switch (cap_get_capType(cap)) {
    case cap_pml4_cap:
        if (cap_pml4_cap_get_capPML4IsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Driving a PML4 cap without an assigned ASID");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

    case cap_frame_cap:
        ret.cap = cap_frame_cap_set_capFMapType(cap, X86_MAPPING_NONE);
        ret.cap = cap_frame_cap_set_capFMappedASID(cap, asidInvalid);
        ret.status = EXCEPTION_NONE;
        return ret;

    default:
        fail("Invalid arch cap type");
    }
}

cap_t Mode_finaliseCap(cap_t cap, bool_t final)
{

    switch (cap_get_capType(cap)) {

    case cap_pml4_cap:
        if (final && cap_pml4_cap_get_capPML4IsMapped(cap)) {
            deleteASID(
                cap_pml4_cap_get_capPML4MappedASID(cap),
                PML4E_PTR(cap_pml4_cap_get_capPML4BasePtr(cap))
                );
        }
        break;

    case cap_pdpt_cap:
        if (final && cap_pdpt_cap_get_capPDPTIsMapped(cap)) {
            unmapPDPT(
                cap_pdpt_cap_get_capPDPTMappedASID(cap),
                cap_pdpt_cap_get_capPDPTMappedAddress(cap),
                PDPTE_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap))
                );
        }
        break;

    case cap_frame_cap:
        if (cap_frame_cap_get_capFMappedASID(cap)) {
            
            if (cap_frame_cap_get_capFMapType(cap) == X86_MAPPING_IOSPACE) {
                unmapIOPage(cap);
                break;
            }

            unmapPage(
                cap_frame_cap_get_capFSize(cap),
                cap_frame_cap_get_capFMappedASID(cap),
                cap_frame_cap_get_capFMappedAddress(cap),
                (void *)cap_frame_cap_get_capFBasePtr(cap)
            );
        }
        break;

    default:
        fail("Invalid arch cap type");
    }

    return cap_null_cap_new();
}

cap_t CONST
Mode_resetMemMapping(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_pml4_cap:
        return cap_pml4_cap_set_capPML4IsMapped(cap, 0);
    default:
        return cap;
    }
}

cap_t Mode_recycleCap(bool_t is_final, cap_t cap)
{

    switch (cap_get_capType(cap)) {

    case cap_pdpt_cap:
        clearMemory((void*)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        if (cap_pdpt_cap_get_capPDPTIsMapped(cap)) {
            unmapPDPT(
                    cap_pdpt_cap_get_capPDPTMappedASID(cap),
                    cap_pdpt_cap_get_capPDPTMappedAddress(cap),
                    PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap))
                    );
        }
        Mode_finaliseCap(cap, is_final);
        if (is_final) {
            return resetMemMapping(cap);
        }
        return cap;

    case cap_pml4_cap:
        clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        Mode_finaliseCap(cap, is_final);
        if (is_final) {
            return resetMemMapping(cap);
        }
        return cap;

    default:
        fail("Invalid arch cap type");
    }
}

bool_t CONST Mode_sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    switch (cap_get_capType(cap_a)) {

    case cap_pml4_cap:
        if (cap_get_capType(cap_b) == cap_pml4_cap) {
            return cap_pml4_cap_get_capPML4BasePtr(cap_a) ==
                   cap_pml4_cap_get_capPML4BasePtr(cap_b);
        }
        return false;
    default:
        return false;
    }
}

word_t
Mode_getObjectSize(word_t t)
{
    switch (t) {
    case seL4_IA32_PML4Object:
        return PML4_SIZE_BITS;

    default:
        fail("Invalid object type");
        return 0;
    }
}

cap_t
Mode_createObject(object_t t, void *regionBase, word_t userSize)
{
    switch (t) {

    case seL4_IA32_4K:
        memzero(regionBase, 1 << pageBitsForSize(IA32_SmallPage));
        return cap_frame_cap_new(
                asidInvalid,        /* capFMappedASID           */
                (word_t)regionBase, /* capFBasePtr              */
                IA32_SmallPage,     /* capFSize                 */
                X86_MAPPING_VSPACE, /* capFMapType              */
                0,                  /* capFMappedAddress        */
                VMReadWrite         /* capFVMRights             */
                );

    case seL4_IA32_LargePage:
        memzero(regionBase, 1 << pageBitsForSize(IA32_LargePage));
        return cap_frame_cap_new(
                asidInvalid,        /* capFMappedASID           */
                (word_t)regionBase, /* capFBasePtr              */
                IA32_LargePage,     /* capFSize                 */
                X86_MAPPING_VSPACE, /* capFMapType              */
                0,                  /* capFMappedAddress        */
                VMReadWrite         /* capFVMRights             */
                );

#ifdef CONFIG_HUGE_PAGE
    case seL4_IA32_HugePage:
        memzero(regionBase, 1 << pageBitsForSize(IA32_HugePage));
        return cap_frame_cap_new(
                asidInvalid,        /* capFMappedASID           */
                (word_t)regionBase, /* capFBasePtr              */
                IA32_HugePage,      /* capFSize                 */
                X86_MAPPING_VSPACE, /* capFMapType              */
                0,                  /* capFMappedAddress        */
                VMReadWrite         /* capFVMRights             */
                );
#endif

    case seL4_IA32_PageTableObject:
        memzero(regionBase, 1 << PT_SIZE_BITS);
        return cap_page_table_cap_new(
                   asidInvalid,            /* capPTMappedASID    */
                   (word_t)regionBase,     /* capPTBasePtr       */
                   0,                      /* capPTIsMapped      */
                   0                       /* capPTMappedAddress */
               );

    case seL4_IA32_PageDirectoryObject:
        memzero(regionBase, 1 << PD_SIZE_BITS);
        return cap_page_directory_cap_new(
                asidInvalid,                /* capPDMappedASID      */
                (word_t)regionBase,         /* capPDBasePtr         */
                0,                          /* capPDIsMapped        */
                0                           /* capPDMappedAddress   */
            );

    case seL4_IA32_PDPTObject:
        memzero(regionBase, 1 << PDPT_SIZE_BITS);
        return cap_pdpt_cap_new(
                asidInvalid,                /* capPDPTMappedASID    */
                (word_t)regionBase,         /* capPDPTBasePtr       */
                0,                          /* capPDPTIsMapped      */
                0                           /* capPDPTMappedAddress */
            );

    case seL4_IA32_PML4Object:
        memzero(regionBase, 1 << PML4_SIZE_BITS);
        copyGlobalMappings(PML4_PTR(regionBase));
        return cap_pml4_cap_new(
                asidInvalid,                /* capPML4MappedASID   */
                (word_t)regionBase,         /* capPML4BasePtr      */
                0                           /* capPML4IsMapped     */
                );

    case seL4_IA32_IOPageTableObject:
        memzero(regionBase, 1 << VTD_PT_SIZE_BITS);
        return cap_io_page_table_cap_new(
                0,
                0,
                0,
                asidInvalid,
                (word_t)regionBase
               );

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
Mode_decodeInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    switch (cap_get_capType(cap)) {
    case cap_pml4_cap:
    case cap_pdpt_cap:
    case cap_page_directory_cap:
    case cap_page_table_cap:
    case cap_frame_cap:
        return decodeX86MMUInvocation(label, length, cptr, slot, cap, extraCaps, buffer);

    default:
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }
}
